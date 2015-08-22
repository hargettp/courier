-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Transport.Sockets.UDP
-- Copyright   :  (c) Phil Hargett 2015
-- License     :  MIT (see LICENSE file)
--
-- Maintainer  :  phil@haphazardhouse.net
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- UDP transport.
--
-----------------------------------------------------------------------------
module Network.Transport.Sockets.UDP (
  newUDPTransport4,
  newUDPTransport6,

  udpSocketResolver4,
  udpSocketResolver6,

  module Network.Transport
) where

-- local imports

import Network.Endpoints
import Network.Transport
import Network.Transport.Sockets

-- external imports

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad

import qualified Data.Map as M

import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
type SocketConnections = TVar (M.Map NS.SockAddr Connection)

newUDPTransport :: NS.Family -> Resolver -> IO Transport
newUDPTransport family resolver = atomically $ do
  vPeers <- newTVar M.empty
  mailboxes <- newTVar M.empty
  return Transport {
    bind = udpBind family resolver,
    dispatch = dispatcher mailboxes,
    connect = udpConnect mailboxes family resolver,
    shutdown = udpShutdown vPeers
  }

newUDPTransport4 :: Resolver -> IO Transport
newUDPTransport4 = newUDPTransport NS.AF_INET

newUDPTransport6 :: Resolver -> IO Transport
newUDPTransport6 = newUDPTransport NS.AF_INET6

udpSocketResolver4 :: Name -> IO [NS.SockAddr]
udpSocketResolver4 = socketResolver4 NS.Datagram

udpSocketResolver6 :: Name -> IO [NS.SockAddr]
udpSocketResolver6 = socketResolver6 NS.Datagram

udpBind :: NS.Family -> Resolver -> Endpoint -> Name -> IO Binding
udpBind family resolver endpoint name = do
  socket <- NS.socket family NS.Datagram NS.defaultProtocol
  address <- resolve1 resolver name
  NS.setSocketOption socket NS.ReuseAddr 1
  NS.bindSocket socket address
  listener <- async $
    finally (receiver socket)
      (udpUnbind socket)
  return Binding {
    bindingName = name,
    unbind = cancel listener
  }
  where
    receiver socket = do
      msg <- udpReceive socket
      -- TODO consider a way of using a message to identify the name of
      -- the endpoint on the other end of the connection
      atomically $ postMessage endpoint msg
      receiver socket

udpUnbind :: NS.Socket -> IO ()
udpUnbind = NS.close

udpConnect :: Mailboxes -> NS.Family -> Resolver -> Endpoint -> Name -> IO Connection
udpConnect mailboxes family resolver _ name = do
  socket <- NS.socket family NS.Datagram NS.defaultProtocol
  address <- resolve1 resolver name
  sender <- async $ finally (writer socket address) (udpDisconnect socket)
  return Connection {
    disconnect = cancel sender
  }
  where
    writer socket address = do
      msg <- atomically $ pullMessage mailboxes name
      udpSend socket address msg
      writer socket address

udpSend :: NS.Socket -> NS.SockAddr -> Message -> IO ()
udpSend socket address message = NSB.sendAllTo socket message address

udpReceive :: NS.Socket -> IO Message
udpReceive socket = do
  (msg,_) <- NSB.recvFrom socket 512
  return msg

udpDisconnect :: NS.Socket -> IO ()
udpDisconnect = NS.close

udpShutdown :: SocketConnections -> IO ()
udpShutdown vPeers = do
  peers <- atomically $ readTVar vPeers
  -- this is how we disconnect incoming connections
  -- we don't have to disconnect  outbound connectinos, because
  -- they should already be disconnected before here
  forM_ (M.elems peers) disconnect
  return ()
