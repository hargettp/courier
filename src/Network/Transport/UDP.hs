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

import Network.Transport
import Network.Transport.Internal
import Network.Transport.Sockets

-- external imports

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception

import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Serialize
import qualified Data.Set as S

import qualified Network.Socket as N
import Network.Socket.ByteString(sendAllTo,recvFrom)

import System.Log.Logger
-- import qualified System.Random as R

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

_log :: String
_log = "transport.udp"

udpScheme :: Scheme
udpScheme = "udp"

data UDPTransport = UDPTransport {
    udpSockets :: S.Set N.Socket,
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
    connConnect = N.socket N.AF_INET N.Datagram N.defaultProtocol,
    connSend = (\s bs -> do
        addr <- lookupAddress $ parseSocketAddress address
        infoM _log $ "Sending via UDP to " ++ (show addr)
        sendAllTo s bs addr
        infoM _log $ "Sent via UDP to " ++ (show addr)),
    connReceive = udpRecvFrom,
    connClose = return ()
    }

newUDPTransport :: Resolver -> IO Transport
newUDPTransport resolver = do
  messengers <- atomically $ newTVar M.empty
  bindings <- atomically $ newTVar M.empty
  inbound <- newMailbox
  dispatch <- async $ dispatcher bindings inbound
  let transport = UDPTransport {
        udpSockets = S.empty,
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
udpBind transport inc name = do
    atomically $ modifyTVar (udpBindings transport) $ \bindings ->
        M.insert name inc bindings
    Just address <- resolve (udpResolver transport) name
    let (_,port) = parseSocketAddress address
    addrinfos <- N.getAddrInfo
                    (Just (N.defaultHints {N.addrFlags = [N.AI_PASSIVE]}))
                    Nothing (Just port)
    let serveraddr = head addrinfos
    sock <-  N.socket (N.addrFamily serveraddr) N.Datagram N.defaultProtocol
    -- have to set this option in case we frequently rebind sockets
    infoM _log $ "Binding to " ++ (show port) ++ " over UDP"
    N.setSocketOption sock N.ReuseAddr 1
    N.bindSocket sock $ N.addrAddress serveraddr
    infoM _log $ "Bound to " ++ (show port) ++ " over UDP"
    rcvr <- async $ udpReceiveSocketMessages sock address (udpInbound transport)
    return $ Right Binding {
        bindingName = name,
        unbind = do
            cancel rcvr
            N.sClose sock
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
          deliver msngr env
          return ()
        Just msngr -> deliver msngr env
    deliver msngr message = atomically $ writeTQueue (messengerOut msngr) message

udpReceiveSocketMessages :: N.Socket -> Address -> Mailbox -> IO ()
udpReceiveSocketMessages sock addr mailbox = catchExceptions (do
    infoM _log $ "Waiting to receive via UDP on " ++ (show addr)
    maybeMsg <- udpReceiveSocketMessage
    infoM _log $ "Received message via UDP on " ++ (show addr)
    case maybeMsg of
        Nothing -> do
            N.sClose sock
            return ()
        Just msg -> do
            atomically $ writeTQueue mailbox msg
            udpReceiveSocketMessages sock addr mailbox) (\e -> do 
                warningM _log $ "Receive error: " ++ (show (e :: SomeException)))
    where
        udpReceiveSocketMessage = do
            maybeMsg <- udpRecvFrom sock 512
            infoM _log $ "Received message"
            return maybeMsg

udpRecvFrom :: N.Socket -> Int -> IO (Maybe B.ByteString)
udpRecvFrom sock count = do
    (bs,addr) <- recvFrom sock count
    infoM _log $ "Received UDP message from " ++ (show addr) ++ ": " ++ (show bs)
    if B.null bs
        then return Nothing
        else return $ Just bs

udpShutdown :: UDPTransport -> IO ()
udpShutdown transport = do
  infoM _log $ "Unbinding transport"
  infoM _log $ "Closing messengers"
  msngrs <- atomically $ readTVar $ udpMessengers transport
  mapM_ closeMessenger $ M.elems msngrs
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
