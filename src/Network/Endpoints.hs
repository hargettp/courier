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
-- by enabling their 'Endpoint's with different 'Transport's without modifying the logic of their
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
  Names,

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
-- > module HelloWorld (
-- >     main
-- > ) where
-- >
-- > -- Just import this package to access the primary APIs
-- > import Network.Endpoints
-- >
-- > -- A specific transport is necessary, however
-- > import Network.Transport.TCP
-- >
-- > -- Needed for serialization
-- > import Data.Serialize
-- >
-- > main :: IO ()
-- > main = do
-- >    let name1 = "endpoint1"
-- >        name2 = "endpoint2"
-- >        resolver = resolverFromList [(name1,"localhost:2000"),
-- >                                (name2,"localhost:2001")]
-- >    transport <- newTCPTransport resolver
-- >    endpoint1 <- newEndpoint [transport]
-- >    endpoint2 <- newEndpoint [transport]
-- >    Right () <- bindEndpoint endpoint1 name1
-- >    Right () <- bindEndpoint endpoint2 name2
-- >    sendMessage_ endpoint1 name2 $ encode "hello world!"
-- >    msg <- receiveMessage endpoint2
-- >    let Right txt = decode msg
-- >        in print (txt :: String)
-- >    Right () <- unbindEndpoint endpoint1 name1
-- >    Right () <- unbindEndpoint endpoint2 name2
-- >    shutdown transport

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
  boundEndpointNames :: Names
  }

{-|
Messages are containers for arbitrary data that may be sent to other 'Network.Endpoints.Endpoint's.
-}
type Message = B.ByteString

{-|
Name for uniquely identifying an 'Endpoint'; suitable for identifying
the target destination for a 'Message'.

-}
newtype Name = Name String deriving (Eq,Ord,Show,Generic)

instance Serialize Name

type Names = TVar (S.Set Name )

{- An 'Envelope' wraps a 'Message' with the 'Name's of the sender and receive -}

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

withName :: Endpoint -> Name -> IO () -> IO ()
withName endpoint origin actor = do
  atomically $ bindName endpoint origin
  finally actor $ atomically $ unbindName endpoint origin

bindName :: Endpoint -> Name -> STM ()
bindName endpoint name = do
  bindings <- readTVar $ boundEndpointNames endpoint
  if S.member name bindings
    then throw $ BindingExists name
    else modifyTVar (boundEndpointNames endpoint) $ S.insert name

unbindName :: Endpoint -> Name -> STM ()
unbindName endpoint name = modifyTVar (boundEndpointNames endpoint) $ S.delete name

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
