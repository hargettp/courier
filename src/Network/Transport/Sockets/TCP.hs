-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Transport.Sockets.TCP
-- Copyright   :  (c) Phil Hargett 2015
-- License     :  MIT (see LICENSE file)
--
-- Maintainer  :  phil@haphazardhouse.net
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- TCP transport.
--
-----------------------------------------------------------------------------
module Network.Transport.Sockets.TCP (
  newTCPTransport4,
  newTCPTransport6,

  tcpSocketResolver4,
  tcpSocketResolver6
  ) where

-- local imports

import Network.Endpoints
import Network.Transport.Sockets

-- external imports

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad

import qualified Data.ByteString as BS
import qualified Data.Map as M
import Data.Serialize

import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

import Debug.Trace
import Text.Printf

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
type SocketConnections = TVar (M.Map NS.SockAddr Connection)

newTCPTransport :: NS.Family -> Resolver -> IO Transport
newTCPTransport family resolver = atomically $ do
  vPeers <- newTVar M.empty
  return Transport {
    bind = tcpBind family resolver,
    connect =  socketConnect $ tcpConnect family resolver,
    shutdown = tcpShutdown vPeers
  }

newTCPTransport4 :: Resolver -> IO Transport
newTCPTransport4 = newTCPTransport NS.AF_INET

newTCPTransport6 :: Resolver -> IO Transport
newTCPTransport6 = newTCPTransport NS.AF_INET6

tcpSocketResolver4 :: Name -> IO [NS.SockAddr]
tcpSocketResolver4 = socketResolver4 NS.Stream

tcpSocketResolver6 :: Name -> IO [NS.SockAddr]
tcpSocketResolver6 = socketResolver6 NS.Stream

tcpBind :: NS.Family -> Resolver -> Endpoint -> Name -> IO Binding
tcpBind family resolver endpoint name = do
  traceM $ printf "binding on %v" (show name)
  vConnections <- atomically $ newTVar M.empty
  listener <- async $ tcpListen family resolver vConnections endpoint name
  return Binding {
    bindingName = name,
    unbind = cancel listener
  }

tcpListen :: NS.Family -> Resolver -> SocketConnections -> Endpoint -> Name -> IO ()
tcpListen family resolver vConnections endpoint name = do
  socket <- listen family NS.Stream resolver name
  traceM $ printf "listening on %v (%v)" (show name) (show socket)
  finally (accept socket vConnections endpoint)
    (tcpUnbind name socket)

accept :: NS.Socket -> SocketConnections -> Endpoint -> IO ()
accept socket vConnections endpoint = do
  traceM $ printf "accepting connections on %v" (show socket)
  (peer,peerAddress) <- NS.accept socket
  connection <- tcpConnection peer
  msngr <- async $ messenger endpoint connection
  let conn = Connection {
    disconnect = cancel msngr
  }
  maybeOldConn <- atomically $ do
    connections <- readTVar vConnections
    let oldConn = M.lookup peerAddress connections
    modifyTVar vConnections $ M.insert peerAddress conn
    return oldConn
  case maybeOldConn of
    Just conn -> disconnect conn
    Nothing -> return ()
  accept socket vConnections endpoint

tcpUnbind :: Name -> NS.Socket -> IO ()
tcpUnbind name socket = do
  traceM $ printf "unbinding on %v (%v)" (show name) (show socket)
  NS.close socket

tcpConnect :: NS.Family -> Resolver -> Endpoint -> Name -> IO SocketConnection
tcpConnect family resolver endpoint name = do
  traceM $ printf "connecting to %v" (show name)
  socket <- NS.socket family NS.Stream NS.defaultProtocol
  address <- resolve1 resolver name
  NS.connect socket address
  traceM $ printf "connected to %v via %v(%v)" (show name) (show socket) (show address)
  conn <- tcpConnection socket
  return conn

tcpConnection :: NS.Socket -> IO SocketConnection
tcpConnection socket = do
  vName <- atomically newEmptyTMVar
  return SocketConnection {
    connectionDestination = vName,
    sendSocketMessage = tcpSend socket,
    receiveSocketMessage = tcpReceive socket,
    disconnectSocket = tcpDisconnect socket
  }

tcpSend :: NS.Socket -> Message -> IO ()
tcpSend socket message = do
  let len = BS.length message
  traceM $ printf "sending length %v" len
  NSB.sendAll socket $ encode len
  traceM $ printf "sending message %v" (show message)
  NSB.sendAll socket message

tcpReceive :: NS.Socket -> IO Message
tcpReceive socket = do
  msg <- readBytesWithLength
  traceM $ printf "received message %v" (show msg)
  return msg
  where
    readBytesWithLength = do
      traceM $ printf "receiving length on %v" (show socket)
      lengthBytes <- readBytes 8 -- TODO must figure out what defines length of an integer in bytes
      case decode lengthBytes of
        Left _ -> do
          traceM $ printf "no data read"
          throw NoDataRead
        Right length -> do
          traceM $ printf "received length of %v" length
          msg <- readBytes length
          return msg
    readBytes = NSB.recv socket

tcpDisconnect :: NS.Socket -> IO ()
tcpDisconnect socket  = do
  NS.close socket
  traceM $ printf "disconnected from %v" (show socket)

tcpShutdown :: SocketConnections -> IO ()
tcpShutdown vPeers = do
  peers <- atomically $ readTVar vPeers
  -- this is how we disconnect incoming connections
  -- we don't have to disconnect  outbound connectinos, because
  -- they should already be disconnected before here
  forM_ (M.elems peers) disconnect
  return ()
