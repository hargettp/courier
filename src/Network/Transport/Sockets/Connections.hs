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
    newSocketVar,

    receiveSocketMessages,
    receiveSocketMessage,
    receiveSocketBytes
    
) where

-- local imports

import Network.Endpoints
import Network.Transport.Internal

-- external imports

import Control.Concurrent.STM
import Control.Exception

import qualified Data.ByteString as B

import Data.Serialize

import Network.Socket hiding (bind, recv, sendTo,shutdown, socket)
import qualified Network.Socket.ByteString  as NSB

import System.Log.Logger

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
_log :: String
_log = "transport.sockets.connections"


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

receiveSocketMessages :: SocketRef -> TVar Bool -> Address -> Mailbox Message -> IO ()
receiveSocketMessages sock done addr mailbox = do
    catchExceptions (do
          infoM _log $ "Waiting to receive on " ++ (show addr)
          maybeMsg <- receiveSocketMessage $ socketRefSocket sock
          infoM _log $ "Received message on " ++ (show addr)
          case maybeMsg of
            Nothing -> do
              sClose $ socketRefSocket sock
              return ()
            Just msg -> do
              atomically $ writeMailbox mailbox msg
          isDone <- atomically $ readTVar done
          if isDone
            then return ()
            else receiveSocketMessages sock done addr mailbox)
          (\e -> do
              isDone <- atomically $ readTVar done
              if isDone
                then return ()
                -- Dropping this message to info, as even well-behaved applications
                -- may generate it...even though it is benign
                else infoM _log $ "Receive error: " ++ (show (e :: SomeException)))

receiveSocketMessage :: Socket -> IO (Maybe B.ByteString)
receiveSocketMessage socket = do
  maybeLen <- receiveSocketBytes socket 8 -- TODO must figure out what defines length of an integer in bytes
  case maybeLen of
    Nothing -> do
      infoM _log $ "No length received"
      return Nothing
    Just len -> do
      maybeMsg <- receiveSocketBytes socket $ msgLength (decode len)
      infoM _log $ "Received message"
      return maybeMsg
  where
    msgLength (Right size) = size
    msgLength (Left err) = error err

receiveSocketBytes :: Socket -> Int -> IO (Maybe B.ByteString)
receiveSocketBytes sock maxBytes = do
    bs <- NSB.recv sock maxBytes
    if B.null bs
        then return Nothing
        else return $ Just bs