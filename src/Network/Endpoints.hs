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
  
 --  * Primary API
  Endpoint,
  newEndpoint,
  
  bindEndpoint,
  unbindEndpoint,
  
  sendMessage,
  receiveMessage,
  
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

import Control.Concurrent.STM
import qualified Data.Map as M

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-|
Endpoints are a locus of communication, used for sending and receive messages.
-}
data Endpoint = Endpoint {
  endpointTransports :: TVar (M.Map Scheme Transport),
  endpointBindings :: TVar (M.Map Address Binding),
  endpointMailbox :: Mailbox
  }

{-|
Create a new 'Endpoint' using the provided transports.
-}
newEndpoint :: [Transport] -> IO Endpoint
newEndpoint trans = do
  transports <- atomically $ newTVar $ M.fromList $ map (\t -> ((scheme t),t)) trans
  bindings <- atomically $ newTVar M.empty
  mailbox <- atomically $ newTQueue
  return Endpoint {
    endpointTransports = transports,
    endpointBindings = bindings,
    endpointMailbox = mailbox
    }

{-|
Binding an 'Endpoint' to an 'Address' prepares the 'Endpoint' to receive
messages sent to the bound address.  Upon success, the result will be @Right ()@, but
if failed, @Left text-of-error-message@.
-}
bindEndpoint :: Endpoint -> Address -> IO (Either String ())
bindEndpoint endpoint address = do 
  transports <- atomically $ readTVar $ endpointTransports endpoint
  let maybeTransport = M.lookup (addressScheme address)  transports
  case maybeTransport of
    Nothing -> return $ Left $ "No transport to handle address: " ++ (show address)
    Just transport -> do
      eitherBinding <- bind transport (endpointMailbox endpoint) address
      case eitherBinding of
        Left err -> return $ Left err
        Right binding -> do 
          atomically $ modifyTVar (endpointBindings endpoint)
            (\bindings -> M.insert address binding bindings)
          return $ Right ()

{-|
Unbind an 'Endpoint' form an 'Address', after which the 'Endpoint' will eventually not 
receive messages sent to that 'Address'. Note that there is no guarantee that after 'Unbind'
succeeds that additional messages to that 'Address' will not be delivered: the only guarantee
is that eventually messages will no longer be delivered.
Upon success, the result will be @Right ()@ but
if failed, @Left text-of-error-message@.
-}
unbindEndpoint :: Endpoint -> Address -> IO (Either String ())
unbindEndpoint endpoint address = do
  bindings <- atomically $ readTVar $ endpointBindings endpoint
  let maybeBinding = M.lookup address bindings
  case maybeBinding of
    Nothing -> return $ Left $ "Endpoint not bound to address: " ++ (show address)
    Just binding -> do 
      unbind binding
      return $ Right ()

{-|
Send a 'Message' to specific 'Address' via the indicated 'Endpoint'. While a successful
response (indicated by returning @Right ()@) indicates that there was no error initiating
transport of the message, success does not guarantee that an 'Endpoint' received the message.
Failure initiating transport is indicated by returning @Left text-of-error-message@.
-}
sendMessage :: Endpoint -> Address -> Message -> IO (Either String ())
sendMessage endpoint address msg  = do
  transports <- atomically $ readTVar $ endpointTransports endpoint
  let maybeTransport = M.lookup (addressScheme address)  transports
  case maybeTransport of
    Nothing -> return $ Left $ "No transport to handle address: " ++ (show address)
    Just transport -> do 
      sendTo transport address msg
      return $ Right ()

{-|
Receive the next 'Message' sent to the 'Endpoint'.
-}
receiveMessage :: Endpoint -> IO Message
receiveMessage endpoint = atomically $ readTQueue $ endpointMailbox endpoint

