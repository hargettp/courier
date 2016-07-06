{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Endpoints
-- Copyright   :  (c) Phil Hargett 2013
-- License     :  MIT (see LICENSE file)
--
-- Maintainer  :  phil@haphazardhouse.net
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- 'Endpoint's are a generalized abstraction for communication between parts of a program,
-- whether on the same physical host or distributed over a network. 'Endpoint's are intended
-- to simplify the development of network-centric applications by providing a small transport-independent
-- message-passing interface, and application writers can independently alter their implementation
-- by enabling their 'Endpoint's with different 'Network.Transport's without modifying the logic of their
-- application that sends / receives 'Message's.
--
-----------------------------------------------------------------------------

module Network.Endpoints (

  -- * How to use courier in an application
  -- $use

  -- * Primary API
  Endpoint(..),
  newEndpoint,

  withName,
  bindName,
  unbindName,
  BindException(..),

  sendMessage,
  broadcastMessage,
  receiveMessage,
  receiveMessageTimeout,
  postMessage,

  -- * Selective message reception
  selectMessage,
  selectMessageTimeout,
  detectMessage,
  detectMessageTimeout,

  -- * Other types
  Envelope(..),
  Message,
  Name(..),

  ) where

-- local imports

-- external imports

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Mailbox
import Control.Concurrent.STM
import Control.Exception

import qualified Data.ByteString as B
import Data.Serialize
import qualified Data.Set as S
import Data.Typeable

import GHC.Generics

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

_log :: String
_log = "transport.endpoints"

-- $use
--
-- A sample of how to use this library:
--
-- > module Main where
-- >
-- >  -- Import this package to manage endpoints
-- > import Network.Endpoints
-- >
-- >  -- A specific transport is necessary, however
-- > import Network.Transport.Sockets.TCP
-- >
-- > -- Needed for serialization
-- > import Data.Serialize
-- >
-- > main :: IO ()
-- > main = do
-- >   -- each endpoint needs a name; since we're using TCP
-- >   -- as our transport, they need to be host/port pairs
-- >   let name1 = Name "localhost:9001"
-- >       name2 = Name "localhost:9002"
-- >       -- the default resolvers just pull apart a name into separate
-- >       -- host and port components; more elaborate resolvers could
-- >       -- perform name lookups or other translations
-- >       resolver = tcpSocketResolver4
-- >   -- we need endpoints for each end of the communication
-- >   endpoint1 <- newEndpoint
-- >   endpoint2 <- newEndpoint
-- >   -- we need a transport to move messages between endpoints
-- >   withTransport (newTCPTransport4 resolver) $ \transport ->
-- >     withEndpoint transport endpoint1 $
-- >       withEndpoint transport endpoint2 $
-- >       -- the first endpoint is just a client, so it needs a name to receive
-- >       -- responses, but does not need a binding since it isn't accept connections
-- >         withName endpoint1 name1 $
-- >           -- the second endpoint is a server, so it needs a binding
-- >           withBinding transport endpoint2 name2 $
-- >             -- a connection between the first endpoint and the name of the second
-- >             -- creates a bi-directional path for messages to flow between the endpoints
-- >             withConnection transport endpoint1 name2 $ do
-- >               sendMessage endpoint1 name2 $ encode "hello world!"
-- >               msg <- receiveMessage endpoint2
-- >               let Right txt = decode msg
-- >                   in print (txt :: String)
-- >
--

{-|
Endpoints are a locus of communication, used for sending and receive messages.
-}
data Endpoint = Endpoint {
  -- | The 'Transport' used by this 'Endpoint'
  -- endpointTransport :: Transport,
  -- | The 'Mailbox' where inbound 'Message's that the 'Endpoint' will receive are queued
  endpointInbound :: Mailbox Message,
  endpointOutbound :: Mailbox Envelope,
  -- | The 'Name's to which the 'Endpoint' is bound
  boundEndpointNames :: TVar (S.Set Name )
  }

{-|
Messages are containers for arbitrary data that may be sent to other 'Network.Endpoints.Endpoint's.
-}
type Message = B.ByteString

{-|
Name for uniquely identifying an 'Endpoint'; suitable for identifying
the target destination for a 'Message'. The specific interpretation of a name
is left to each 'Network.Transport.Transport'
-}
newtype Name = Name String deriving (Eq,Ord,Show,Generic)

instance Serialize Name

{-|
 An 'Envelope' wraps a 'Message' with the 'Name's of the destination for the message and (optionally)
the origin.
-}

data Envelope = Envelope {
  messageOrigin :: Maybe Name,
  messageDestination :: Name,
  envelopeMessage :: Message
} deriving (Eq,Show)

{-|
Create a new 'Endpoint' using the provided transports.
-}
newEndpoint :: IO Endpoint
newEndpoint = atomically $ do
  inbound <- newMailbox
  outbound <- newMailbox
  names <- newTVar S.empty
  return Endpoint {
    endpointInbound = inbound,
    endpointOutbound = outbound,
    boundEndpointNames = names
    }

{-|
Declare an 'Endpoint' as having the specified 'Name' while the supplied function executes. This can
often be useful for establishing the 'Name' of a client or initiator of a 'Network.Transport.Connection',
without requiring the client also have a 'Network.Transport.Binding'.

-}
withName :: Endpoint -> Name -> IO a -> IO a
withName endpoint origin actor = do
  atomically $ bindName endpoint origin
  finally actor $ atomically $ unbindName endpoint origin

{-|
Establish 'Name' as one of the 'boundEndpointNames' for an 'Endpoint'. Throws 'BindingExists' if
the 'Endpoint' is already bound to the 'Name'.
-}
bindName :: Endpoint -> Name -> STM ()
bindName endpoint name = do
  bindings <- readTVar $ boundEndpointNames endpoint
  if S.member name bindings
    then throw $ BindingExists name
    else modifyTVar (boundEndpointNames endpoint) $ S.insert name

{-|
Remove 'Name' as one of the 'boundEndpointNames' for an 'Endpoint'. Throws 'BindingDoesNotExist'
if the 'Endpoint' is not bound to the 'Name'.
-}
unbindName :: Endpoint -> Name -> STM ()
unbindName endpoint name = modifyTVar (boundEndpointNames endpoint) $ \bindings ->
  if S.member name bindings
    then S.delete name bindings
    else throw $ BindingDoesNotExist name

{-|
Exceptions generated when `Network.Transport.bind`ing a 'Name'.
-}
data BindException =
  BindingExists Name |
  BindingDoesNotExist Name
  deriving (Show,Typeable)

instance Exception BindException

{-|
Send a 'Message' to specific 'Name' via the indicated 'Endpoint'.
-}
sendMessage :: Endpoint -> Name -> Message -> IO ()
sendMessage endpoint name msg  = atomically $
    writeMailbox (endpointOutbound endpoint) $ Envelope Nothing name msg

{-|
Helper for sending a single 'Message' to several 'Endpoint's.
-}
broadcastMessage :: Endpoint -> [Name] -> Message -> IO ()
broadcastMessage endpoint names msg = mapM_ (\name -> sendMessage endpoint name msg) names

{-|
Receive the next 'Message' sent to the 'Endpoint', blocking until a message is available.
-}
receiveMessage :: Endpoint -> IO Message
receiveMessage endpoint = atomically $ readMailbox $ endpointInbound endpoint

{-|
Wait for a message to be received within the timeout, blocking until either a message
is available or the timeout has occurred.  If a message was available, returns @Just message@,
but returns @Nothing@ if no message available before the timeout occurred.
-}
receiveMessageTimeout :: Endpoint -> Int -> IO (Maybe Message)
receiveMessageTimeout endpoint delay = do
  resultOrTimeout <- race (receiveMessage endpoint) (threadDelay delay)
  case resultOrTimeout of
    Left result -> return $ Just result
    Right () -> return Nothing

{-|
Posts a 'Message' directly to an 'Endpoint', without use of a transport. This
may be useful for applications that prefer to use the 'Endpoint''s 'Mailbox'
as a general queue of ordered messages.
-}
postMessage :: Endpoint -> Message -> STM ()
postMessage endpoint = writeMailbox (endpointInbound endpoint)

{-|
Select the next available message in the 'Endpoint' 'Mailbox' matching
the supplied test function, or blocking until one is available. This function
differs from 'receiveMessage' in that it supports out of order message reception.
-}
selectMessage :: Endpoint -> (Message -> Maybe v) -> IO v
selectMessage endpoint testFn = atomically $ selectMailbox (endpointInbound endpoint) testFn

{-|
Wait for a message to be selected within the timeout, blocking until either a message
is available or the timeout has occurred.  If a message was available, returns @Just message@,
but returns @Nothing@ if no message available before the timeout occurred. Like
'selectMessage', this function enables out of order message reception.
-}
selectMessageTimeout :: Endpoint -> Int -> (Message -> Maybe v) -> IO (Maybe v)
selectMessageTimeout endpoint delay testFn = do
  resultOrTimeout <- race (selectMessage endpoint testFn) (threadDelay delay)
  case resultOrTimeout of
    Left result -> return $ Just result
    Right () -> return Nothing

{-|
Find a 'Message' in the 'Endpoint' 'Mailbox' matching the supplied
test function, or block until one is available.  Note that any such message
is left in the mailbox, and thus repeated calls to this function could find the
message if it is not consumed immediately.
-}
detectMessage :: Endpoint -> (Message -> Maybe v) -> IO v
detectMessage endpoint testFn = atomically $ findMailbox (endpointInbound endpoint) testFn

{-|
Find a 'Message' in the 'Endpoint' 'Mailbox' matching the supplied
test function, or block until either one is available or the timeout expires.
Note that any such message is left in the mailbox, and thus repeated calls
to this function could find the message if it is not consumed immediately.
-}
detectMessageTimeout :: Endpoint -> Int -> (Message -> Maybe v) -> IO (Maybe v)
detectMessageTimeout endpoint delay testFn = do
  resultOrTimeout <- race (detectMessage endpoint testFn) (threadDelay delay)
  case resultOrTimeout of
    Left result -> return $ Just result
    Right () -> return Nothing
