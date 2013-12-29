{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Transport.TCP
-- Copyright   :  (c) Phil Hargett 2013
-- License     :  MIT (see LICENSE file)
-- 
-- Maintainer  :  phil@haphazardhouse.net
-- Stability   :  experimental
-- Portability :  non-portable (uses STM)
--
-- TCP transports deliver messages to other 'Network.Endpoints.Endpoint's using TCP/IP.
--
-- Each TCP transport manages both socket bindings and connections on behalf of
-- 'Endpoint's, dynamically opening / closing new sockets as needed to deliver
-- messages to other 'Endpoint's using TCP transports.
-----------------------------------------------------------------------------

module Network.Transport.TCP (
  newTCPTransport
  ) where

-- local imports

import Network.Transport
import Network.Transport.Sockets

-- external imports

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception

import qualified Data.Map as M
import Data.Serialize
import qualified Data.Set as S

import Network.Socket (sClose,accept)
import Network.Simple.TCP hiding (accept)

import System.Log.Logger

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

_log :: String
_log = "transport.tcp"

data TCPTransport = TCPTransport {
  tcpListeners :: TVar (M.Map ServiceName Socket),
  tcpMessengers :: TVar (M.Map Address Messenger),  
  tcpBindings :: TVar (M.Map Name Mailbox),
  tcpInbound :: Mailbox,
  tcpDispatchers :: S.Set (Async ()),
  tcpResolver :: Resolver
  }

newTCPConnection :: Address -> IO Connection
newTCPConnection address = do
  sock <- atomically $ newEmptyTMVar
  let (host,port) = parseSocketAddress address
  return Connection {
    connAddress = address,
    connSocket = sock,
    connConnect = do
        (s,_) <- connectSock host port
        return s,
    connSend = send,
    connReceive = recv
    }

{-|
Create a new 'Transport' suitable for sending messages over TCP/IP.  There can
be multiple instances of these 'Transport's: 'Network.Endpoints.Endpoint' using
different instances will still be able to communicate, provided they use
correct TCP/IP addresses (or hostnames) for communication.
-}
newTCPTransport :: Resolver -> IO Transport
newTCPTransport resolver = do 
  listeners <- atomically $ newTVar M.empty
  messengers <- atomically $ newTVar M.empty
  bindings <- atomically $ newTVar M.empty
  inbound <- newMailbox
  dispatch <- async $ dispatcher bindings inbound
  let transport = TCPTransport {
        tcpListeners = listeners,
        tcpMessengers = messengers,
        tcpBindings = bindings,
        tcpInbound = inbound,
        tcpDispatchers = S.fromList [dispatch],
        tcpResolver = resolver
        }
  return Transport {
      scheme = tcpScheme,
      handles = tcpHandles transport,
      bind = tcpBind transport,
      sendTo = tcpSendTo transport,
      shutdown = tcpShutdown transport
      }

--------------------------------------------------------------------------------

tcpScheme :: Scheme
tcpScheme = "tcp"

tcpHandles :: TCPTransport -> Name -> IO Bool
tcpHandles transport name = do 
  resolved <- resolve (tcpResolver transport) name
  return $ isJust resolved
  where
    isJust (Just _) = True
    isJust _ = False

tcpBind :: TCPTransport -> Mailbox -> Name -> IO (Either String Binding)
tcpBind transport inc name = do  
  atomically $ modifyTVar (tcpBindings transport) $ \bindings ->
    M.insert name inc bindings
  Just address <- resolve (tcpResolver transport) name
  let (_,port) = parseSocketAddress address
  listener <- async $ do 
    infoM _log $ "Binding to address " ++ (show address)
    tcpListen address port
  return $ Right Binding {
    bindingName = name,
    unbind = tcpUnbind listener address
    }
  where
    tcpListen address port = 
        listen HostAny port $ \(socket,_) -> 
            catch (do 
                    tcpAccept address socket)
                    (\e -> do 
                           warningM _log $ "Listen error: " ++ (show (e :: SomeException)))
    tcpAccept address socket = do
      infoM _log $ "Listening for connections on " ++ (show address) ++ ": " ++ (show socket)
      (client,clientAddress) <- accept socket
      _ <- async $ tcpDispatch address client clientAddress
      tcpAccept address socket
    tcpDispatch address client socketAddress = do
      infoM _log $ "Accepted connection on " ++ (show address)
      identity <- tcpIdentify client socketAddress
      case identity of
        Nothing -> sClose client
        Just (IdentifyMessage clientAddress) -> do
          infoM _log $ "Identified " ++ (show clientAddress)
          clientSocket <- atomically $ newTMVar client
          newConn <- newTCPConnection clientAddress
          let conn = newConn {connSocket = clientSocket}
          msngr <- newMessenger conn (tcpInbound transport)
          found <- atomically $ do 
            msngrs <- readTVar $ tcpMessengers transport
            return $ M.lookup clientAddress msngrs
          case found of
            Just _ -> do
              infoM _log $ "Already have messenger for " ++ (show clientAddress)
              closeMessenger msngr
            Nothing -> do
              addMessenger transport clientAddress msngr
    tcpIdentify client clientAddress = do
      infoM _log $ "Awaiting identity from " ++ (show clientAddress)
      maybeMsg <- receiveMessage client
      case maybeMsg of
        Nothing -> return Nothing
        Just bytes -> do
          let msg = decode bytes
          case msg of
            Left _ -> return Nothing
            Right message -> return $ Just message
    tcpUnbind listener address = do 
      infoM _log $ "Unbinding from port " ++ (show address)
      cancel listener

tcpSendTo :: TCPTransport -> Name -> Message -> IO ()
tcpSendTo transport name msg = do
  isLocal <- local
  if isLocal
    then return ()
    else remote
  where
    local = do
      found <- atomically $ do
        bindings <- readTVar $ tcpBindings transport
        return $ M.lookup name bindings
      case found of
        Nothing -> return False
        Just mbox -> do
          atomically $ writeTQueue mbox msg
          return True
    remote = do 
      Just address <- resolve (tcpResolver transport) name
      let env = encode $ Envelope {
            envelopeDestination = name,
            envelopeContents = msg
            }
      amsngr <- atomically $ do
        msngrs <- readTVar $ tcpMessengers transport
        return $ M.lookup address msngrs
      case amsngr of
        Nothing -> do
          msngrs <- atomically $ readTVar $ tcpMessengers transport
          infoM _log $ "No messenger for " ++ (show address) ++ " in " ++ (show msngrs)
          socketVar <- atomically $ newEmptyTMVar
          newConn <- newTCPConnection address
          let conn = newConn {connSocket = socketVar}
          msngr <- newMessenger conn (tcpInbound transport)
          addMessenger transport address msngr
          identifyAll msngr
          deliver msngr env
          return ()
        Just msngr -> deliver msngr env
    deliver msngr message = atomically $ writeTQueue (messengerOut msngr) message
    identifyAll msngr = do
      bindings <- atomically $ readTVar $ tcpBindings transport
      boundAddresses <- mapM (resolve $ tcpResolver transport) (M.keys bindings)
      let uniqueAddresses = S.toList $ S.fromList boundAddresses
      mapM_ (identify msngr) uniqueAddresses
    identify msngr maybeUniqueAddress= do
      case maybeUniqueAddress of
        Nothing -> return()
        Just uniqueAddress -> deliver msngr $ encode $ IdentifyMessage uniqueAddress

tcpShutdown :: TCPTransport -> IO ()
tcpShutdown transport = do
  infoM _log $ "Closing messengers"
  msngrs <- atomically $ readTVar $ tcpMessengers transport
  mapM_ closeMessenger $ M.elems msngrs
  infoM _log $ "Closing listeners"
  listeners <- atomically $ readTVar $ tcpListeners transport
  mapM_ sClose $ M.elems listeners
  infoM _log $ "Closing dispatcher"
  mapM_ cancel $ S.toList $ tcpDispatchers transport
  mapM_ wait $ S.toList $ tcpDispatchers transport

addMessenger :: TCPTransport -> Address -> Messenger -> IO ()
addMessenger transport address msngr = do
  msngrs <- atomically $ do
        modifyTVar (tcpMessengers transport) $ \msngrs -> M.insert address msngr msngrs
        msngrs <- readTVar (tcpMessengers transport)
        return msngrs
  infoM _log $ "Added messenger to " ++ (show address) ++ "; messengers are " ++ (show msngrs)
