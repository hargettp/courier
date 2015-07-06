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

  sendMessage,
  broadcastMessage,
  receiveMessage,
  receiveMessageTimeout,
  postMessage,
  pullMessage,

  -- * Selective message reception
  selectMessage,
  selectMessageTimeout,
  detectMessage,
  detectMessageTimeout,
  dispatchMessage,
  dispatchMessageTimeout,

  -- * Other types
  Message,
  Name(..),
  Names,

  -- * Transport
  Transport(..),
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
  withConnection,
  withConnection2,
  withConnection3,
  withConnection4

  ) where

-- local imports

-- external imports

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Mailbox
import Control.Concurrent.STM

import Control.Exception

import qualified Data.Map as M

import qualified Data.ByteString as B
import Data.Serialize
import qualified Data.Set as S

import GHC.Generics

import System.Log.Logger

import Text.Printf

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
  endpointTransport :: Transport,
  -- | The 'Mailbox' where inbound 'Message's that the 'Endpoint' will receive are queued
  endpointInbound :: Mailbox Message,
  -- | The 'M.Map' of 'Mailbox'es where 'Message'es intended for other 'Endpoint's are
  --   queued for the 'Transport' to eventually send
  endpointOutbound :: Mailboxes,
  -- | The 'Name's to which the 'Endpoint' is bound
  endpointNames :: Names
  }

type Mailboxes = TVar (M.Map Name (Mailbox Message))

{-|
Create a new 'Endpoint' using the provided transports.
-}
newEndpoint :: Transport -> IO Endpoint
newEndpoint transport = atomically $ do
  inbound <- newMailbox
  outbound <- newTVar M.empty
  names <- newTVar S.empty
  return Endpoint {
    endpointTransport = transport,
    endpointInbound = inbound,
    endpointOutbound = outbound,
    endpointNames = names
    }

{-|
A 'Transport' defines a specific method for establishing connections
between 'Endpoint's.
-}
data Transport = Transport {
  bind :: Endpoint -> Name -> IO (Either String Binding),
  connect :: Endpoint -> Name -> IO (Either String Connection),
  shutdown :: IO ()
  }

withEndpoint :: Transport -> (Endpoint -> IO ()) -> IO ()
withEndpoint transport communicator =
  finally
    (newEndpoint transport >>= communicator)
    (shutdown transport)

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

withBinding :: Endpoint -> Name -> IO () -> IO ()
withBinding endpoint name listener = do
  let transport = endpointTransport endpoint
  errorOrBinding <- bind transport endpoint name
  case errorOrBinding of
    Left err -> do
      errorM _log $ printf "Binding on %v encountered error: %v" (show name) err
      return ()
    Right binding -> do
      finally listener $ unbind binding

withBinding2 :: (Endpoint,Name) -> (Endpoint,Name) -> IO () -> IO ()
withBinding2 (endpoint1,name1) (endpoint2,name2) fn =
  withBinding endpoint1 name1 $
    withBinding endpoint2 name2 fn

withBinding3 :: (Endpoint,Name) -> (Endpoint,Name) -> (Endpoint,Name) -> IO () -> IO ()
withBinding3 (endpoint1,name1) (endpoint2,name2) (endpoint3,name3) fn =
  withBinding endpoint1 name1 $
    withBinding endpoint2 name2 $
      withBinding endpoint3 name3 fn

withBinding4 :: (Endpoint,Name) -> (Endpoint,Name) -> (Endpoint,Name) -> (Endpoint,Name) -> IO () -> IO ()
withBinding4 (endpoint1,name1) (endpoint2,name2) (endpoint3,name3) (endpoint4,name4) fn =
  withBinding endpoint1 name1 $
    withBinding endpoint2 name2 $
      withBinding endpoint3 name3 $
        withBinding endpoint4 name4 fn

{-
Connections are pathways for sending messages to an 'Endpoint' bound to a specific 'Name'
-}
data Connection = Connection {
  connectionDestination :: Name,
  disconnect :: IO ()
}

withConnection :: Endpoint -> Name -> IO () -> IO ()
withConnection endpoint name communicator = do
  let transport = endpointTransport endpoint
  errorOrConnection <- connect transport endpoint name
  case errorOrConnection of
    Left err -> do
      errorM _log $ printf "Connection to %v encountered error: %v" (show name) err
      return ()
    Right connection -> finally communicator $ disconnect connection

withConnection2 ::Endpoint -> Name -> Name -> IO () -> IO ()
withConnection2 endpoint name1 name2 communicator =
  withConnection endpoint name1 $
    withConnection endpoint name2 communicator

withConnection3 ::Endpoint -> Name -> Name -> Name -> IO () -> IO ()
withConnection3 endpoint name1 name2 name3 communicator =
  withConnection endpoint name1 $
    withConnection endpoint name2 $
      withConnection endpoint name3 communicator

withConnection4 ::Endpoint -> Name -> Name -> Name -> Name -> IO () -> IO ()
withConnection4 endpoint name1 name2 name3 name4 communicator = 
  withConnection endpoint name1 $
    withConnection endpoint name2 $
      withConnection endpoint name3 $
        withConnection endpoint name4 communicator

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

{-|
Send a 'Message' to specific 'Name' via the indicated 'Endpoint'.
-}
sendMessage :: Endpoint -> Name -> Message -> IO ()
sendMessage endpoint name msg  = atomically $ do
    outbound <- readTVar $ endpointOutbound endpoint
    mailbox <- case M.lookup name outbound of
      Nothing -> do
        mailbox <- newMailbox
        modifyTVar (endpointOutbound endpoint) $ M.insert name mailbox
        return mailbox
      Just mailbox -> return mailbox
    writeMailbox mailbox msg

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
postMessage endpoint message = writeMailbox (endpointInbound endpoint) message

{-
Pull a 'Message' intended for a destination name from an 'Endpoint' directly,
without the use of a 'Transport'
-}
pullMessage :: Endpoint -> Name -> STM Message
pullMessage endpoint destination = do
  outbound <- readTVar $ endpointOutbound endpoint
  case M.lookup destination outbound of
    Nothing -> retry
    Just mailbox -> readMailbox mailbox

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

{-|
Dispatch the next available message in the 'Endpoint' 'Mailbox' matching
the supplied test function, or blocking until one is available. Once a
matching message is found, handle the message with the supplied handler
and return any result obtained. This function differs from 'receiveMessage'
in that it supports out of order message reception.
-}
dispatchMessage :: Endpoint -> (Message -> Maybe v) -> (v -> IO r) -> IO r
dispatchMessage endpoint = handleMailbox (endpointInbound endpoint)

dispatchMessageTimeout :: Endpoint -> Int -> (Message -> Maybe v) -> (v -> IO r) -> IO (Maybe r)
dispatchMessageTimeout endpoint delay testFn handleFn = do
  resultOrTimeout <- race (dispatchMessage endpoint testFn handleFn) (threadDelay delay)
  case resultOrTimeout of
    Left result -> return $ Just result
    Right () -> return Nothing
