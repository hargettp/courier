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
--
-- There is no reuse of sockets on the sending side, so while messages will be 
-- received on a known bound port, the remote sending port will vary arbitrarily.
-- 
-- This transport only reads at most 512 bytes from incoming packets: constraining
-- the packet size avoids fragmentation.  Applications using this transport should take
-- responsibility for fragmentation, reassembly, retransmission of lost packets,
-- and congestion control.
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

newUDPTransport :: Resolver -> IO Transport
newUDPTransport resolver = do
  messengers <- atomically $ newTVar M.empty
  bindings <- atomically $ newTVar M.empty
  sockets <- newSocketBindings
  inbound <- atomically $ newMailbox
  dispatch <- async $ dispatcher bindings inbound
  let transport = SocketTransport {
        socketMessengers = messengers,
        socketBindings = bindings,
        socketConnection = newUDPConnection,
        socketMessenger = newUDPMessenger,
        socketInbound = inbound,
        socketDispatchers = S.fromList [dispatch],
        socketResolver = resolver
        }
  return Transport {
      scheme = udpScheme,
      handles = udpHandles transport,
      bind = udpBind transport sockets,
      sendTo = socketSendTo transport,
      shutdown = udpShutdown transport sockets
      }

udpHandles :: SocketTransport -> Name -> IO Bool
udpHandles transport name = do 
  resolved <- resolve (socketResolver transport) name
  return $ isJust resolved
  where
    isJust (Just _) = True
    isJust _ = False

udpBind :: SocketTransport -> SocketBindings -> Mailbox Message -> Name -> IO (Either String Binding)
udpBind transport sockets inc name = do
    atomically $ modifyTVar (socketBindings transport) $ \bindings ->
        M.insert name inc bindings
    Just address <- resolve (socketResolver transport) name
    bindAddress sockets address $ do
        let (_,port) = parseSocketAddress address
        addrinfos <- N.getAddrInfo
                        (Just (N.defaultHints {N.addrFlags = [N.AI_PASSIVE,N.AI_NUMERICSERV]}))
                        Nothing (Just port)
        let ipv4Addrs = filter (\sockAddr ->
                                 (N.addrFamily sockAddr == N.AF_INET)
                                 && (N.addrSocketType sockAddr == N.Datagram))
                        addrinfos
        infoM _log $ "Socket addresses are " ++ (show addrinfos)
        infoM _log $ "IPv4 addresses are " ++ (show ipv4Addrs)                        
        let serveraddr = head ipv4Addrs
        sock <-  N.socket (N.addrFamily serveraddr) N.Datagram N.defaultProtocol
        -- have to set this option in case we frequently rebind sockets
        infoM _log $ "Binding to " ++ (show port) ++ " over UDP"
        N.setSocketOption sock N.ReuseAddr 1
        N.bindSocket sock $ N.addrAddress serveraddr
        infoM _log $ "Bound to " ++ (show port) ++ " over UDP"
        rcvr <- async $ udpReceiveSocketMessages sock address (socketInbound transport)
        return (sock,rcvr)
    return $ Right Binding {
        bindingName = name,
        unbind = do
            infoM _log $ "Unbinding from UDP port " ++ (show address)
            unbindAddress sockets address
            infoM _log $ "Unbound from UDP port " ++ (show address)
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
    connClose = do
        maybeSocket <- atomically $ tryTakeTMVar sock
        case maybeSocket of
            Just s -> N.sClose s
            Nothing -> return ()
        return ()
    }

newUDPMessenger :: Connection -> Mailbox Message -> IO Messenger
newUDPMessenger conn mailbox = do
    msngr <- newMessenger conn mailbox
    return msngr

udpReceiveSocketMessages :: N.Socket -> Address -> Mailbox Message -> IO ()
udpReceiveSocketMessages sock addr mailbox = catchExceptions 
    (do
        infoM _log $ "Waiting to receive via UDP on " ++ (show addr)
        maybeMsg <- udpReceiveSocketMessage
        infoM _log $ "Received message via UDP on " ++ (show addr)
        case maybeMsg of
            Nothing -> do
                N.sClose sock
                return ()
            Just msg -> do
                atomically $ writeMailbox mailbox msg
                udpReceiveSocketMessages sock addr mailbox) 
    (\e -> do
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

udpShutdown :: SocketTransport -> SocketBindings -> IO ()
udpShutdown transport sockets = do
  infoM _log $ "Unbinding transport"
  closeBindings sockets
  infoM _log $ "Closing messengers"
  msngrs <- atomically $ readTVar $ socketMessengers transport
  mapM_ closeMessenger $ M.elems msngrs
  infoM _log $ "Closing dispatcher"
  mapM_ cancel $ S.toList $ socketDispatchers transport
  mapM_ wait $ S.toList $ socketDispatchers transport

