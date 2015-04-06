-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Transport.Sockets.Connections
-- Copyright   :  (c) Phil Hargett 2014
-- License     :  MIT (see LICENSE file)
-- 
-- Maintainer  :  phil@haphazardhouse.net
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- (..... module description .....)
--
-----------------------------------------------------------------------------

module Network.Transport.Sockets.Connections (
    Connection(..),

    disconnected,
    connected,
    setConnectedSocket,
    closeConnectedSocket,
    forceCloseConnectedSocket,

    SocketRef(..),
    SocketVar,
    newSocketVar
) where

-- local imports

import Network.Endpoints

-- external imports

import Control.Concurrent.STM

import qualified Data.ByteString as B

import Network.Socket hiding (bind, recv, sendTo,shutdown, socket)
import qualified Network.Socket.ByteString  as NSB


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-|
A connection specializes the use of a transport for a particular
destination.
-}
data Connection = Connection {
  connAddress :: Address,
  connSocket  :: SocketVar,
  connConnect :: IO Socket,
  connSend    :: Socket -> B.ByteString -> IO (),
  connReceive :: Socket -> Int -> IO (Maybe B.ByteString),
  connClose   :: IO ()
  }

connected :: SocketVar -> STM SocketRef
connected var = do
    state <- readTMVar var
    case socketStateSocket state of
        Nothing -> retry
        Just socket -> return SocketRef {
            socketRefVersion = socketStateVersion state,
            socketRefSocket = socket
        }

disconnected :: SocketVar -> STM ()
disconnected var = do
    state <- readTMVar var
    case socketStateSocket state of
        Nothing -> return ()
        Just _ -> do
            retry

setConnectedSocket :: SocketVar -> Socket -> IO ()
setConnectedSocket var socket = atomically $ do
    state <- readTMVar var
    -- atomically $ putTMVar (connSocket conn) $ SocketRef 0 socket
    putTMVar var $ state {
        socketStateVersion = 1 + (socketStateVersion state),
        socketStateSocket = Just socket
        }

closeConnectedSocket :: SocketVar -> SocketRef -> IO ()
closeConnectedSocket var ref = do
    maybeOldSocket <- atomically $ do
        state <- readTMVar var
        case socketStateSocket state of
            Nothing -> return Nothing
            Just socket -> do
                if (socketRefVersion ref) == (socketStateVersion state) then
                    -- atomically $ putTMVar (connSocket conn) $ SocketRef 0 socket
                    putTMVar var $ state {
                        socketStateVersion = 1 + (socketStateVersion state),
                        socketStateSocket = Nothing
                        }
                else
                    return ()
                return $ socketStateSocket state
    case maybeOldSocket of
        Nothing -> return ()
        Just socket -> sClose socket

forceCloseConnectedSocket :: SocketVar -> IO ()
forceCloseConnectedSocket var = do
    maybeOldSocket <- atomically $ do
        state <- readTMVar var
        case socketStateSocket state of
            Nothing -> return Nothing
            Just socket -> do
                -- atomically $ putTMVar (connSocket conn) $ SocketRef 0 socket
                putTMVar var $ state {
                    socketStateVersion = 1 + (socketStateVersion state),
                    socketStateSocket = Nothing
                    }
                return $ socketStateSocket state
    case maybeOldSocket of
        Nothing -> return ()
        Just socket -> sClose socket

type SocketVar = TMVar SocketState

newSocketVar :: STM SocketVar
newSocketVar = newTMVar $ SocketState {
        socketStateVersion = 0,
        socketStateSocket = Nothing
    }

data SocketRef = SocketRef {
    socketRefVersion :: Integer,
    socketRefSocket  :: Socket
}


data SocketState = SocketState {
    socketStateVersion :: Integer,
    socketStateSocket  :: Maybe Socket
    }
