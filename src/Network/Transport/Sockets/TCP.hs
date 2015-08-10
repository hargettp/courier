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
  newTCPTransport
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

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

newTCPTransport :: NS.Family -> Resolver -> Endpoint -> IO Transport
newTCPTransport family resolver endpoint = atomically $ do
  vBindings <- newTVar M.empty
  vConnections <- newTVar M.empty
  vPeers <- newTVar M.empty
  return Transport {
    bind = tcpBind family resolver vBindings,
    connect =  socketConnect vConnections (tcpConnect family resolver) ,
    shutdown = tcpShutdown vBindings vConnections vPeers
  }

tcpBind :: NS.Family -> Resolver -> SocketBindings -> Endpoint -> Name -> IO Binding
tcpBind family resolver vBindings endpoint name = do
  alreadyBound <- atomically $ do
    bindings <- readTVar vBindings
    return $ M.lookup name bindings
  case alreadyBound of
    Nothing -> do
      listener <- async tcpListener
      vListener <- atomically $ newTMVar listener
      atomically $ modifyTVar vBindings $ M.insert name $ SocketBinding {
        socketBindingName = name,
        socketListener = vListener
      }
  return Binding {
    bindingName = name,
    unbind = tcpUnbind
  }

tcpUnbind :: IO ()
tcpUnbind = return ()

tcpConnect :: NS.Family -> Resolver -> Endpoint -> Name -> IO SocketConnection
tcpConnect family resolver endpoint name = do
  socket <- NS.socket family NS.Stream NS.defaultProtocol
  address <- resolve1 resolver name
  NS.connect socket address
  return SocketConnection {
    disconnectSocketConnection = tcpDisconnect socket,
    sendSocketMessage = tcpSend socket,
    receiveSocketMessage = tcpReceive socket
  }

tcpListener :: IO ()
tcpListener = return ()

tcpSend :: NS.Socket -> Message -> IO ()
tcpSend socket message = do
  NSB.sendAll socket $ encode (BS.length message)
  NSB.sendAll socket message

tcpReceive :: NS.Socket -> IO Message
tcpReceive socket = return BS.empty
  where
    readLength = do
      lengthBytes <- readBytes 8 -- TODO must figure out what defines length of an integer in bytes
      case decode lengthBytes of
        Left _ -> throw NoDataRead
        Right length -> readBytes length
    readBytes = NSB.recv socket

tcpDisconnect :: NS.Socket -> IO ()
tcpDisconnect = NS.close

tcpShutdown :: SocketBindings -> SocketConnections -> SocketConnections -> IO ()
tcpShutdown vBindings vConnections vPeers = do
  bindings <- atomically $ readTVar vBindings
  connections <- atomically $ readTVar vConnections
  peers <- atomically $ readTVar vPeers
  forM_ (M.elems bindings) $ \binding -> do
    listener <- atomically $ takeTMVar $ socketListener binding
    cancel listener
