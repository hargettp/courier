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
  newUDPTransport,
  newUDPTransport6,

  lookupAddresses,
  lookupUDPAddress,
  lookupWildcardUDPAddress
  ) where

-- local imports

import Network.Transport
import Network.Transport.Internal
import Network.Transport.Sockets
import Network.Transport.Sockets.Addresses

-- external imports

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception

import qualified Data.ByteString as B
import qualified Data.Map as M

import qualified Network.Socket as NS
import Network.Socket.ByteString(sendAllTo,recvFrom)

import System.Log.Logger

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

_log :: String
_log = "transport.udp"

udpScheme :: Scheme
udpScheme = "udp"

lookupUDPAddress :: Address -> NS.Family -> IO NS.SockAddr
lookupUDPAddress address family = lookupAddress family NS.Datagram address

lookupWildcardUDPAddress :: Address -> NS.Family -> IO NS.SockAddr
lookupWildcardUDPAddress address family = lookupWildcardAddress family NS.Datagram address

newUDPTransport :: Resolver -> IO Transport
newUDPTransport resolver = newSocketTransport resolver udpScheme (udpBind NS.AF_INET) (newUDPConnection NS.AF_INET) newUDPMessenger

newUDPTransport6 :: Resolver -> IO Transport
newUDPTransport6 resolver = newSocketTransport resolver udpScheme (udpBind NS.AF_INET6) (newUDPConnection NS.AF_INET6) newUDPMessenger

udpBind :: NS.Family -> SocketTransport -> SocketBindings -> Mailbox Message -> Name -> IO (Either String Binding)
udpBind family transport sockets inc name = do
    atomically $ modifyTVar (socketBindings transport) $ \bindings ->
        M.insert name inc bindings
    Just address <- resolve (socketResolver transport) name
    bindAddress sockets address $ do
        sockaddr <- lookupWildcardUDPAddress address family
        sock <-  NS.socket family NS.Datagram NS.defaultProtocol
        -- have to set this option in case we frequently rebind sockets
        infoM _log $ "Binding to " ++ (show address) ++ " over UDP"
        NS.setSocketOption sock NS.ReuseAddr 1
        NS.bindSocket sock sockaddr
        infoM _log $ "Bound to " ++ (show address) ++ " over UDP"
        rcvr <- async $ udpReceiveSocketMessages sock address (socketInbound transport)
        return (sock,rcvr)
    return $ Right Binding {
        bindingName = name,
        unbind = do
            infoM _log $ "Unbinding from UDP port " ++ (show address)
            unbindAddress sockets address
            infoM _log $ "Unbound from UDP port " ++ (show address)
    }

newUDPConnection :: NS.Family -> Address -> IO Connection
newUDPConnection family address = do
  var <- atomically newSocketVar
  return Connection {
    connAddress = address,
    connSocket = var,
    connConnect = NS.socket family NS.Datagram NS.defaultProtocol,
    connSend = (\s bs -> do
        addr <- lookupUDPAddress address family
        infoM _log $ "Sending via UDP to " ++ (show addr)
        sendAllTo s bs addr
        infoM _log $ "Sent via UDP to " ++ (show addr)),
    connReceive = udpRecvFrom,
    connClose = do
        forceCloseConnectedSocket var
        {-
        maybeSocket <- atomically $ tryTakeTMVar sock
        case maybeSocket of
            Just s -> NS.sClose $ socketRefSocket s
            Nothing -> return ()
        -}
        return ()
    }

newUDPMessenger :: Bindings -> Resolver -> Connection -> Mailbox Message -> IO Messenger
newUDPMessenger _ _ conn mailbox = do
    msngr <- newMessenger conn mailbox
    return msngr

udpReceiveSocketMessages :: NS.Socket -> Address -> Mailbox Message -> IO ()
udpReceiveSocketMessages sock addr mailbox = catchExceptions 
    (do
        infoM _log $ "Waiting to receive via UDP on " ++ (show addr)
        maybeMsg <- udpReceiveSocketMessage
        infoM _log $ "Received message via UDP on " ++ (show addr)
        case maybeMsg of
            Nothing -> do
                NS.sClose sock
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

udpRecvFrom :: NS.Socket -> Int -> IO (Maybe B.ByteString)
udpRecvFrom sock count = do
    (bs,addr) <- recvFrom sock count
    infoM _log $ "Received UDP message from " ++ (show addr) ++ ": " ++ (show bs)
    if B.null bs
        then return Nothing
        else return $ Just bs
