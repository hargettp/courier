{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Transport.UDP
-- Copyright   :  (c) Phil Hargett 2013
-- License     :  MIT (see LICENSE file)
-- 
-- Maintainer  :  phil@haphazardhouse.net
-- Stability   :  experimental
-- Portability :  non-portable (uses STM)
--
-- UDP transports deliver messages to other 'Network.Endpoints.Endpoint's using UDP/IP.
--
-- Each UDP transport manages socket bindings on behalf of
-- 'Endpoint's, dynamically opening / closing new sockets as needed to deliver
-- messages to other 'Endpoint's using UDP transports.
-----------------------------------------------------------------------------

module Network.Transport.UDP (
  newUDPTransport
  ) where

-- local imports

import Network.Transport.Sockets
import Network.Transport

-- external imports

import Control.Concurrent.Async
import Control.Concurrent.STM

import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Serialize
import qualified Data.Set as S

import Network.Socket (
    ServiceName,
    Socket,
    SockAddr(..),
    iNADDR_ANY,
    socket,
    sClose,
    Family(..),
    SocketType(..),
    defaultProtocol)
import qualified Network.Socket as N
import Network.Socket.ByteString(sendAll,recv)

import System.Log.Logger

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

_log :: String
_log = "transport.udp"

udpScheme :: Scheme
udpScheme = "udp"

data UDPTransport = UDPTransport {
  udpListeners :: TVar (M.Map ServiceName Socket),
  udpMessengers :: TVar (M.Map Address Messenger),  
  udpBindings :: TVar (M.Map Name Mailbox),
  udpInbound :: Mailbox,
  udpDispatchers :: S.Set (Async ()),
  udpResolver :: Resolver
  }

newUDPConnection :: Address -> IO Connection
newUDPConnection address = do
  sock <- atomically newEmptyTMVar
  return Connection {
    connAddress = address,
    connSocket = sock,
    connConnect = socket AF_INET6 Datagram defaultProtocol,
    connSend = sendAll,
    connReceive = udpReceive
    }
  where
      udpReceive sock byteCount = do
          bytes <- recv sock byteCount
          if B.null bytes
            then return Nothing 
            else return $ Just bytes

newUDPTransport :: Resolver -> IO Transport
newUDPTransport resolver = do 
  listeners <- atomically $ newTVar M.empty
  messengers <- atomically $ newTVar M.empty
  bindings <- atomically $ newTVar M.empty
  inbound <- newMailbox
  dispatch <- async $ dispatcher bindings inbound
  let transport = UDPTransport {
        udpListeners = listeners,
        udpMessengers = messengers,
        udpBindings = bindings,
        udpInbound = inbound,
        udpDispatchers = S.fromList [dispatch],
        udpResolver = resolver
        }
  return Transport {
      scheme = udpScheme,
      handles = udpHandles transport,
      bind = udpBind transport,
      sendTo = udpSendTo transport,
      shutdown = udpShutdown transport
      }

udpHandles :: UDPTransport -> Name -> IO Bool
udpHandles transport name = do 
  resolved <- resolve (udpResolver transport) name
  return $ isJust resolved
  where
    isJust (Just _) = True
    isJust _ = False

udpBind :: UDPTransport -> Mailbox -> Name -> IO (Either String Binding)
udpBind transport _ name = do
    Just address <- resolve (udpResolver transport) name
    let (_,port) = parseSocketAddress address
    sock <- socket AF_INET Datagram defaultProtocol
    let portnum = read port
    infoM _log $ "Binding to " ++ (show portnum) ++ " over UDP"
    N.bind sock $ SockAddrInet (N.PortNum portnum) iNADDR_ANY
    return $ Right Binding {
        bindingName = name,
        unbind = sClose sock
    }

udpSendTo :: UDPTransport -> Name -> Message -> IO ()
udpSendTo transport name msg = do
  isLocal <- local
  if isLocal
    then return ()
    else remote
  where
    local = do
      found <- atomically $ do
        bindings <- readTVar $ udpBindings transport
        return $ M.lookup name bindings
      case found of
        Nothing -> return False
        Just mbox -> do
          atomically $ writeTQueue mbox msg
          return True
    remote = do 
      Just address <- resolve (udpResolver transport) name
      let env = encode $ Envelope {
            envelopeDestination = name,
            envelopeContents = msg
            }
      amsngr <- atomically $ do
        msngrs <- readTVar $ udpMessengers transport
        return $ M.lookup address msngrs
      case amsngr of
        Nothing -> do
          msngrs <- atomically $ readTVar $ udpMessengers transport
          infoM _log $ "No messenger for " ++ (show address) ++ " in " ++ (show msngrs)
          socketVar <- atomically $ newEmptyTMVar
          newConn <- newUDPConnection address
          let conn = newConn {connSocket = socketVar}
          msngr <- newMessenger conn (udpInbound transport)
          addMessenger transport address msngr
          identifyAll msngr
          deliver msngr env
          return ()
        Just msngr -> deliver msngr env
    deliver msngr message = atomically $ writeTQueue (messengerOut msngr) message
    identifyAll msngr = do
      bindings <- atomically $ readTVar $ udpBindings transport
      boundAddresses <- mapM (resolve $ udpResolver transport) (M.keys bindings)
      let uniqueAddresses = S.toList $ S.fromList boundAddresses
      mapM_ (identify msngr) uniqueAddresses
    identify msngr maybeUniqueAddress= do
      case maybeUniqueAddress of
        Nothing -> return()
        Just uniqueAddress -> deliver msngr $ encode $ IdentifyMessage uniqueAddress

udpShutdown :: UDPTransport -> IO ()
udpShutdown transport = do
  infoM _log $ "Closing messengers"
  msngrs <- atomically $ readTVar $ udpMessengers transport
  mapM_ closeMessenger $ M.elems msngrs
  infoM _log $ "Closing listeners"
  listeners <- atomically $ readTVar $ udpListeners transport
  mapM_ sClose $ M.elems listeners
  infoM _log $ "Closing dispatcher"
  mapM_ cancel $ S.toList $ udpDispatchers transport
  mapM_ wait $ S.toList $ udpDispatchers transport

addMessenger :: UDPTransport -> Address -> Messenger -> IO ()
addMessenger transport address msngr = do
  msngrs <- atomically $ do
        modifyTVar (udpMessengers transport) $ \msngrs -> M.insert address msngr msngrs
        msngrs <- readTVar (udpMessengers transport)
        return msngrs
  infoM _log $ "Added messenger to " ++ (show address) ++ "; messengers are " ++ (show msngrs)
