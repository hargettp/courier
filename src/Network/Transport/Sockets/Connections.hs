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
    state <- readTVar var
    case socketStateSocket state of
        Nothing -> retry
        Just socket -> return SocketRef {
            socketRefVersion = socketStateVersion state,
            socketRefSocket = socket
        }

disconnected :: SocketVar -> STM ()
disconnected var = do
    state <- readTVar var
    case socketStateSocket state of
        Nothing -> return ()
        Just _ -> do
            retry

setConnectedSocket :: SocketVar -> Socket -> IO ()
setConnectedSocket var socket = atomically $ do
    state <- readTVar var
    writeTVar var $ state {
        socketStateVersion = 1 + (socketStateVersion state),
        socketStateSocket = Just socket
        }

closeConnectedSocket :: SocketVar -> SocketRef -> IO ()
closeConnectedSocket var ref = do
    maybeOldSocket <- atomically $ do
        state <- readTVar var
        case socketStateSocket state of
            Nothing -> return Nothing
            Just _ -> do
                if (socketRefVersion ref) == (socketStateVersion state) then
                    writeTVar var $ state {
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
        state <- readTVar var
        case socketStateSocket state of
            Nothing -> return Nothing
            Just _ -> do
                writeTVar var $ state {
                    socketStateVersion = 1 + (socketStateVersion state),
                    socketStateSocket = Nothing
                    }
                return $ socketStateSocket state
    case maybeOldSocket of
        Nothing -> return ()
        Just socket -> sClose socket

type SocketVar = TVar SocketState

newSocketVar :: STM SocketVar
newSocketVar = newTVar $ SocketState {
        socketStateVersion = 0,
        socketStateSocket = Nothing
    }

data SocketRef = SocketRef {
    socketRefVersion :: Integer,
    socketRefSocket  :: Socket
} deriving (Eq,Show)

data SocketState = SocketState {
    socketStateVersion :: Integer,
    socketStateSocket  :: Maybe Socket
    } deriving (Eq,Show)