{-# LANGUAGE ExistentialQuantification #-}

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
  Endpoint,
  newEndpoint,
  
  bindEndpoint,
  unbindEndpoint,
  
  sendMessage,
  sendMessage_,
  broadcastMessage,
  broadcastMessage_,                 
  receiveMessage,
  receiveMessageTimeout,
  
  -- * Transports
  {-|
  Transports define specific implementations of message-passing techniques (e.g.,
  memory-based, TCP, UDP, HTTP, etc.). Typical use of the 'Endpoint's does not
  require direct use of 'Transport's, beyond creating specific 'Transport's (such as
  found in "Network.Transport.Memory" and "Network.Transport.TCP") and adding
  them to an 'Endpoint'.
  -}
  module Network.Transport
  
  ) where

-- local imports

import Network.Transport

-- external imports

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM

import qualified Data.Map as M

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- $use
-- 
-- A sample of how to use this library:
-- 
-- > -- Just import this package to access the primary APIs
-- > import Network.Endpoints
-- >
-- > -- A specific transport is necessary, however
-- > import Network.Transport.TCP
-- > 
-- > helloWorld :: IO ()
-- > helloWorld = do
-- >   let name1 = "endpoint1"b
-- >       name2 = "endpoint2"
-- >       resolver = resolverFromList [(name1,"localhost:2000"),
-- >                                    (name2,"localhost:2001")]
-- >   transport <- newTCPTransport resolver
-- >   endpoint1 <- newEndpoint [transport]
-- >   endpoint2 <- newEndpoint [transport]
-- >   Right () <- bindEndpoint endpoint1 name1
-- >   Right () <- bindEndpoint endpoint2 name2
-- >   sendMessage endpoint1 name2 $ encode "hello world!"
-- >   msg <- receiveMessage endpoint2
-- >   print msg
-- >   shutdown transport

{-|
Endpoints are a locus of communication, used for sending and receive messages.
-}
data Endpoint = Endpoint {
  endpointTransports :: TVar [Transport],
  endpointBindings :: TVar (M.Map Name Binding),
  endpointMailbox :: Mailbox
  }

{-|
Create a new 'Endpoint' using the provided transports.
-}
newEndpoint :: [Transport] -> IO Endpoint
newEndpoint trans = do
  transports <- atomically $ newTVar trans
  bindings <- atomically $ newTVar M.empty
  mailbox <- atomically $ newTQueue
  return Endpoint {
    endpointTransports = transports,
    endpointBindings = bindings,
    endpointMailbox = mailbox
    }

{-|
Binding an 'Endpoint' to a 'Name' prepares the 'Endpoint' to receive
messages sent to the bound name.  Upon success, the result will be @Right ()@, but
if failed, @Left text-of-error-message@.
-}
bindEndpoint :: Endpoint -> Name -> IO (Either String ())
bindEndpoint endpoint name = do 
  maybeTransport <- findTransport endpoint name
  case maybeTransport of
    Nothing -> return $ Left $ "No transport to handle name: " ++ (show name)
    Just transport -> do
      eitherBinding <- bind transport (endpointMailbox endpoint) name
      case eitherBinding of
        Left err -> return $ Left err
        Right binding -> do 
          atomically $ modifyTVar (endpointBindings endpoint)
            (\bindings -> M.insert name binding bindings)
          return $ Right ()

{-|
Unbind an 'Endpoint' from a 'Name', after which the 'Endpoint' will eventually not 
receive messages sent to that 'Name'. Note that there is no guarantee that after 'Unbind'
succeeds that additional messages to that 'Name' will not be delivered: the only guarantee
is that eventually messages will no longer be delivered.
Upon success, the result will be @Right ()@ but
if failed, @Left text-of-error-message@.
-}
unbindEndpoint :: Endpoint -> Name -> IO (Either String ())
unbindEndpoint endpoint name = do
  bindings <- atomically $ readTVar $ endpointBindings endpoint
  let maybeBinding = M.lookup name bindings
  case maybeBinding of
    Nothing -> return $ Left $ "Endpoint not bound to address: " ++ (show name)
    Just binding -> do 
      unbind binding
      return $ Right ()

{-|
Send a 'Message' to specific 'Name' via the indicated 'Endpoint'. While a successful
response (indicated by returning @Right ()@) indicates that there was no error initiating
transport of the message, success does not guarantee that an 'Endpoint' received the message.
Failure initiating transport is indicated by returning @Left text-of-error-message@.
-}
sendMessage :: Endpoint -> Name -> Message -> IO (Either String ())
sendMessage endpoint name msg  = do
  maybeTransport <- findTransport endpoint name
  case maybeTransport of
    Nothing -> return $ Left $ "No transport to handle name: " ++ (show name)
    Just transport -> do 
      sendTo transport name msg
      return $ Right ()

{-|
A variant of 'sendMessage' for use when the return value can be ignored.

-}
sendMessage_ :: Endpoint -> Name -> Message -> IO ()
sendMessage_ endpoint name msg = do
  _ <- sendMessage_ endpoint name msg
  return ()

{-|
Helper for sending a single 'Message' to several 'Endpoint's.
-}
broadcastMessage :: Endpoint -> [Name] -> Message -> IO [(Either String ())]
broadcastMessage endpoint names msg = do
  mapM (\name -> sendMessage endpoint name msg) names

{-|
Variant of 'broadcastMessage' that ignores the results of sending.

-}
broadcastMessage_ :: Endpoint -> [Name] -> Message -> IO ()
broadcastMessage_ endpoint names msg = do
  _ <- broadcastMessage endpoint names msg
  return ()

{-|
Receive the next 'Message' sent to the 'Endpoint', blocking until a message is available.
-}
receiveMessage :: Endpoint -> IO Message
receiveMessage endpoint = atomically $ readTQueue $ endpointMailbox endpoint

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

findTransport :: Endpoint -> Name -> IO (Maybe Transport)
findTransport endpoint name = do
  transports <- atomically $ readTVar $ endpointTransports endpoint
  findM canHandle transports
    where
      canHandle transport = (handles transport) name
      findM mf (a:as) = do
        result <- mf a
        if result
          then return $ Just a
          else findM mf as
      findM _ [] = return Nothing
