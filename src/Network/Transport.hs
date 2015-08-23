{-# LANGUAGE DeriveDataTypeable     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Transport
-- Copyright   :  (c) Phil Hargett 2015
-- License     :  MIT (see LICENSE file)
--
-- Maintainer  :  phil@haphazardhouse.net
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- A 'Transport' abstracts the details of message delivery, and defines the interfaces
-- that specific 'Transport' implementations should provide in order to move messages
-- between 'Endpoint's.
--
-- The definition of a transport is deliberately low-level in nature.  Unless a specific
-- transport describes itself as supporting features like guaranteed delivery, applications
-- should NOT assume that message delivery is reliable.
--
-- For example, if a sender sends a message to a name that has not yet been bound, then
-- immediately waits on the response for that message, then the application may hang,
-- as the original message may have been dropped.
--
-- However, many application may find it easier to push features such as reliable
-- message delivery into a custom transport, leaving the application only having
-- to concern itself with the messages being delivered rather than how they arrive.
--
-- The main abstractions common to all transports:
--
-- * 'Endpoint's may not receive messages until either 'bind' has been called on an available
-- 'Transport', or a 'bindName' has been called on the 'Endpoint'. The latter is typically useful
-- for 'Endpoint's that originate connections but do not accept them directly.
-- * A 'Connection' defines a bi-directional pathway for messages to flow between 'Endpoint's. The initiator
-- of the connection is a client, and the destination for the connection is a server. Server 'Endpoint's
-- may have to have called 'bind' on the 'Transport' before being able to accept connections or receive messages;
-- a client only has to have called 'bindName' in order to receive responses from the server.
-- * Client's proactively attempt to maintain 'Connection's to the server; in the event a server breaks the connection
-- (other because of a crash or deliberate exit), the client will continue to restore the connection.
-- * 'Connection's essentially define the topology over which messages will propagate between 'Endpoint's.
--
-----------------------------------------------------------------------------
module Network.Transport (

  -- * Transport
  Transport(..),
  TransportException(..),

  Dispatcher(..),
  dispatcher,

  Mailboxes,
  pullMessage,
  dispatchMessage,

  withTransport,

  withEndpoint,
  withEndpoint2,
  withEndpoint3,
  withEndpoint4,

  Binding(..),
  withBinding,
  withBinding2,
  withBinding3,
  withBinding4,

  Connection(..),
  ConnectException(..),
  withConnection,
  withConnection2,
  withConnection3,
  withConnection4,

) where

-- local imports

import Network.Endpoints

-- external imports

import Control.Concurrent.Async
import Control.Concurrent.Mailbox
import Control.Concurrent.STM
import Control.Exception

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Typeable

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-|
A 'Transport' defines a specific method for establishing connections
between 'Endpoint's.
-}
data Transport = Transport {
  bind :: Endpoint -> Name -> IO Binding,
  dispatch :: Endpoint -> IO Dispatcher,
  connect :: Endpoint -> Name -> IO Connection,
  shutdown :: IO ()
  }

data Dispatcher = Dispatcher {
  stop :: IO ()
}

{-|
An exception encountered by a 'Transport'.
-}
data TransportException =
  NoDataRead |
  DataUnderflow
  deriving (Show,Typeable)

instance Exception TransportException

{-|
Wraps in a continually repeating call to 'dispatchMessage' in a 'Dispatcher' so that dispatching
can be stopped when no longer needed.
-}
dispatcher :: Mailboxes -> Endpoint -> IO Dispatcher
dispatcher mailboxes endpoint = do
  d <- async disp
  return Dispatcher {
    stop = cancel d
  }
  where
    disp = do
      atomically $ do
        envelope <- readMailbox $ endpointOutbound endpoint
        let name = messageDestination envelope
            msg = envelopeMessage envelope
        dispatchMessage mailboxes name msg
      disp

{-|
A mutable 'Map' of 'Name's to 'Mailbox'es of 'Message's.
-}
type Mailboxes = TVar (M.Map Name (Mailbox Message))

{-
Pull a 'Message' intended for a destination name from a 'Mailboxes' directly,
without the use of a 'Transport'
-}
pullMessage :: Mailboxes -> Name -> STM Message
pullMessage mailboxes destination = do
  outbound <- readTVar mailboxes
  case M.lookup destination outbound of
    Nothing -> retry
    Just mailbox -> readMailbox mailbox

{-|
A simple function to multiplex messages (each wrapped in an 'Envelope') in the 'endpointOutbound'
mailbox of an 'Endpoint' to one or more 'Mailboxes' by extract the 'messageDestination' from the 'Envelope' and
finding or creating a 'Mailbox' containing messages only for that destination. This is often useful for 'Transport's,
who then only have to monitor a specific 'Mailbox' to know when there are messages to send to a particular destination.
-}
dispatchMessage :: Mailboxes -> Name -> Message -> STM ()
dispatchMessage mailboxes name message = do
  outbound <- readTVar mailboxes
  mailbox <- case M.lookup name outbound of
    Nothing -> do
      mailbox <- newMailbox
      modifyTVar mailboxes $ M.insert name mailbox
      return mailbox
    Just mailbox -> return mailbox
  writeMailbox mailbox message

{-|
Within the body of the function, ensures that 'Message's are dispatched as necessary.
-}
withTransport :: Transport -> Endpoint -> IO () -> IO ()
withTransport transport endpoint actor = do
  d <- dispatch transport endpoint
  finally actor $
    finally (stop d) $
      shutdown transport

withEndpoint :: Transport -> (Endpoint -> IO ()) -> IO ()
withEndpoint transport actor = do
    endpoint <- newEndpoint
    withTransport transport endpoint (actor endpoint)

withEndpoint2 :: Transport -> (Endpoint -> Endpoint -> IO ()) -> IO ()
withEndpoint2 transport fn =
  withEndpoint transport $ \endpoint1 ->
    withEndpoint transport $ \endpoint2 -> fn endpoint1 endpoint2

withEndpoint3 :: Transport -> (Endpoint -> Endpoint -> Endpoint -> IO () ) -> IO ()
withEndpoint3 transport fn =
  withEndpoint transport $ \endpoint1 ->
    withEndpoint transport $ \endpoint2 ->
      withEndpoint transport $ \endpoint3 -> fn endpoint1 endpoint2 endpoint3

withEndpoint4 :: Transport -> (Endpoint -> Endpoint -> Endpoint -> Endpoint -> IO () ) -> IO ()
withEndpoint4 transport fn =
  withEndpoint2 transport $ \endpoint1 endpoint2 ->
    withEndpoint2 transport $ \endpoint3 endpoint4 -> fn endpoint1 endpoint2 endpoint3 endpoint4

{-|
Bindings are a site for receiving messages on a particular 'Name'
through a 'Transport'.
-}
data Binding = Binding {
  bindingName :: Name,
  unbind :: IO ()
  }

withBinding :: Transport -> Endpoint -> Name -> IO () -> IO ()
withBinding transport endpoint name actor = do
  atomically $ do
    bindings <- readTVar $ boundEndpointNames endpoint
    if S.member name bindings
      then throw $ BindingExists name
      else modifyTVar (boundEndpointNames endpoint) $ S.insert name
  binding <- bind transport endpoint name
  finally actor $ do
    unbind binding
    atomically $ modifyTVar (boundEndpointNames endpoint) $ S.delete name

withBinding2 :: Transport -> (Endpoint,Name) -> (Endpoint,Name) -> IO () -> IO ()
withBinding2 transport (endpoint1,name1) (endpoint2,name2) fn =
  withBinding transport endpoint1 name1 $
    withBinding transport endpoint2 name2 fn

withBinding3 :: Transport -> (Endpoint,Name) -> (Endpoint,Name) -> (Endpoint,Name) -> IO () -> IO ()
withBinding3 transport (endpoint1,name1) (endpoint2,name2) (endpoint3,name3) fn =
  withBinding transport endpoint1 name1 $
    withBinding transport endpoint2 name2 $
      withBinding transport endpoint3 name3 fn

withBinding4 :: Transport -> (Endpoint,Name) -> (Endpoint,Name) -> (Endpoint,Name) -> (Endpoint,Name) -> IO () -> IO ()
withBinding4 transport (endpoint1,name1) (endpoint2,name2) (endpoint3,name3) (endpoint4,name4) fn =
  withBinding transport endpoint1 name1 $
    withBinding transport endpoint2 name2 $
      withBinding transport endpoint3 name3 $
        withBinding transport endpoint4 name4 fn

{-
Connections are pathways for sending messages to an 'Endpoint' bound to a specific 'Name'
-}
data Connection = Connection {
  disconnect :: IO ()
}

data ConnectException =
  ConnectionExists Name |
  ConnectionDoesNotExist Name |
  ConnectionHasNoBoundPeer Name
  deriving (Show,Typeable)

instance Exception ConnectException

withConnection :: Transport -> Endpoint -> Name -> IO () -> IO ()
withConnection transport endpoint name actor = do
  connection <- connect transport endpoint name
  finally actor $ disconnect connection

withConnection2 :: Transport -> Endpoint -> Name -> Name -> IO () -> IO ()
withConnection2 transport endpoint name1 name2 communicator =
  withConnection transport endpoint name1 $
    withConnection transport endpoint name2 communicator

withConnection3 :: Transport -> Endpoint -> Name -> Name -> Name -> IO () -> IO ()
withConnection3 transport endpoint name1 name2 name3 communicator =
  withConnection transport endpoint name1 $
    withConnection transport endpoint name2 $
      withConnection transport endpoint name3 communicator

withConnection4 :: Transport -> Endpoint -> Name -> Name -> Name -> Name -> IO () -> IO ()
withConnection4 transport endpoint name1 name2 name3 name4 communicator =
  withConnection transport endpoint name1 $
    withConnection transport endpoint name2 $
      withConnection transport endpoint name3 $
        withConnection transport endpoint name4 communicator
