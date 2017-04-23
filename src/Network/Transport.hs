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
  withClient,
  withServer,

  Binding(..),
  withBinding,
  withBinding2,
  withBinding3,
  withBinding4,

  Connection(..),
  withConnection,
  withConnection2,
  withConnection3,
  withConnection4,

  withConnections,
  withCompleteNetwork

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

{-|
A 'Dispatcher' encapsulates a 'Transport's interaction with an 'Endpoint' for outbound message flow.
-}
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

{-|
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
withTransport :: IO Transport -> (Transport -> IO a) -> IO a
withTransport factory fn = do
  transport <- factory
  finally (fn transport)
   (shutdown transport)

{-|
Within the body of the function, ensure that there is a 'Dispatcher' for the 'Endpoint'.

-}
withEndpoint :: Transport -> Endpoint -> IO a  -> IO a
withEndpoint transport endpoint fn = do
  d <- dispatch transport endpoint
  finally fn
    (stop d)

{-|
Bindings are a site for receiving messages on a particular 'Name'
through a 'Transport'.
-}
data Binding = Binding {
  bindingName :: Name,
  unbind :: IO ()
  }

{-|
A helper for ensuring there is a 'Binding' of a specific 'Endpoint' to a specific 'Name'
on the provided 'Transport' during a function.

-}
withBinding :: Transport -> Endpoint -> Name -> IO a -> IO a
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

{-|
A helper for ensuring there are 'Binding's of a specific 'Endpoint' to specific 'Name's
on the provided 'Transport' during a function.
-}
withBinding2 :: Transport -> (Endpoint,Name) -> (Endpoint,Name) -> IO a -> IO a
withBinding2 transport (endpoint1,name1) (endpoint2,name2) fn =
  withBinding transport endpoint1 name1 $
    withBinding transport endpoint2 name2 fn

{-|
A helper for ensuring there are 'Binding's of a specific 'Endpoint' to specific 'Name's
on the provided 'Transport' during a function.
-}
withBinding3 :: Transport -> (Endpoint,Name) -> (Endpoint,Name) -> (Endpoint,Name) -> IO a -> IO a
withBinding3 transport (endpoint1,name1) (endpoint2,name2) (endpoint3,name3) fn =
  withBinding transport endpoint1 name1 $
    withBinding transport endpoint2 name2 $
      withBinding transport endpoint3 name3 fn

{-|
A helper for ensuring there are 'Binding's of a specific 'Endpoint' to specific 'Name's
on the provided 'Transport' during a function.
-}
withBinding4 :: Transport -> (Endpoint,Name) -> (Endpoint,Name) -> (Endpoint,Name) -> (Endpoint,Name) -> IO a -> IO a
withBinding4 transport (endpoint1,name1) (endpoint2,name2) (endpoint3,name3) (endpoint4,name4) fn =
  withBinding transport endpoint1 name1 $
    withBinding transport endpoint2 name2 $
      withBinding transport endpoint3 name3 $
        withBinding transport endpoint4 name4 fn

{-|
Connections are bi-directional pathways for exchanging 'Message's with another 'Endpoint'
that is bound to a specific 'Name' on a shared 'Transport'.
-}
data Connection = Connection {
  disconnect :: IO ()
}

{-|
A helper for ensuring that a 'Connection' is maintained during execution of a function.
-}
withConnection :: Transport -> Endpoint -> Name -> IO a -> IO a
withConnection transport endpoint name fn = do
  connection <- connect transport endpoint name
  finally fn $ disconnect connection

{-|
A helper for ensuring that 2 'Connection's are maintained during execution of a function.
-}
withConnection2 :: Transport -> Endpoint -> Name -> Name -> IO a -> IO a
withConnection2 transport endpoint name1 name2 = withConnections transport endpoint [name1,name2]

{-|
A helper for ensuring that 3 'Connection's are maintained during execution of a function.
-}
withConnection3 :: Transport -> Endpoint -> Name -> Name -> Name -> IO a -> IO a
withConnection3 transport endpoint name1 name2 name3 = withConnections transport endpoint [name1,name2,name3]

{-|
A helper for ensuring that 4 'Connection's are maintained during execution of a function.
-}
withConnection4 :: Transport -> Endpoint -> Name -> Name -> Name -> Name -> IO a -> IO a
withConnection4 transport endpoint name1 name2 name3 name4 = withConnections transport endpoint [name1,name2,name3,name4]

--
--  Various helpers
--
withConnections :: Transport -> Endpoint -> [Name] -> IO a -> IO a
withConnections _ _ [] fn = fn
withConnections transport endpoint (destination:destinations) fn =
  withConnection transport endpoint destination $
    withConnections transport endpoint destinations fn

{-|
This is a helper designed to create a complete network, where there are
enough connections to ensure every endpoint has a connection to every other endpoint.
-}
withCompleteNetwork :: Transport -> [Name] -> Endpoint -> Name -> IO a -> IO a
withCompleteNetwork _ [] _ _ fn = fn
withCompleteNetwork transport(destination:destinations) endpoint origin fn =
  if destination == origin
    then withConnections transport endpoint destinations fn
    else withCompleteNetwork transport destinations endpoint origin fn

{-|
Helper for easily creating clients capable of sending messages on the `Transport`
and receiving messages at the provided `Name`
-}
withClient :: IO Transport -> Name -> (Endpoint -> IO a) -> IO a
withClient transportFactory name clientFn =
  withTransport transportFactory $ \transport -> do
    endpoint <- newEndpoint
    withEndpoint transport endpoint $
      withName endpoint name $
        clientFn endpoint

{-|
Helper for easily creating servers listening messages sent to the specified `Name`
over the provided `Transport`
-}
withServer :: IO Transport -> Name -> (Transport -> Endpoint -> IO a) -> IO a
withServer transportFactory name serverFn =
  withTransport transportFactory $ \transport -> do
    endpoint <- newEndpoint
    withEndpoint transport endpoint $
      withBinding transport endpoint name $
        serverFn transport endpoint
