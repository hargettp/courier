-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Transport.Sockets.Messengers
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

module Network.Transport.Sockets.Messengers (
    Messenger(..),
    newMessenger,
    closeMessenger,

    receiveSocketMessage,
    receiveSocketBytes
) where

-- local imports

import Network.Endpoints

import Network.Transport.Internal
import Network.Transport.Sockets.Addresses
import Network.Transport.Sockets.Connections

-- external imports
import Control.Concurrent.Async
import Control.Exception
import Control.Concurrent.STM

import qualified Data.ByteString as B

import Data.Serialize

import Network.Socket hiding (bind, recv, sendTo,shutdown, socket)
import qualified Network.Socket.ByteString  as NSB

import System.Log.Logger

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
_log :: String
_log = "transport.sockets.messengers"

{-|
A messenger is a facility that actually uses the mechanisms of a transport
(and more specifically, of a connection on a transport) to deliver and receive
messages. The messenger uses 'Mailbox'es internally so that the sending/receiving
happens asynchronously, allowing applications to move on without regard for
when any send / receive action actually completes.
-}
data Messenger = Messenger {
    messengerDone       :: TVar Bool,
    messengerOut        :: Mailbox Message,
    messengerAddress    :: Address,
    messengerSender     :: Async (),
    messengerConnector  :: Async (),
    messengerReceiver   :: Async (),
    messengerConnection :: Connection
    }

instance Show Messenger where
  show msngr = "Messenger(" ++ (show $ messengerAddress msngr) ++ ")"

newMessenger :: Connection -> Mailbox Message -> IO Messenger
newMessenger conn inc = do
  out <- atomically $ newMailbox
  done <- atomically $ newTVar False
  sndr <- async $ sender conn done out
  connr <- async $ connector conn out done
  rcvr <- async $ receiver conn done inc
  return Messenger {
    messengerDone = done,
    messengerOut = out,
    messengerAddress = connAddress conn,
    messengerSender = sndr,
    messengerConnector = connr,
    messengerReceiver = rcvr,
    messengerConnection = conn
    }

sender :: Connection -> TVar Bool -> Mailbox Message -> IO ()
sender conn done mailbox = sendMessages
  where
    sendMessages = do
      infoM _log $ "Waiting for connection to " ++ (show $ connAddress conn)
      socket <- atomically $ connected $ connSocket conn
      catchExceptions (do
                infoM _log $ "Waiting to send to " ++ (show $ connAddress conn)
                msg <- atomically $ readMailbox mailbox
                infoM _log $ "Sending message to " ++ (show $ connAddress conn)
                (connSend conn) (socketRefSocket socket) msg
                )
            (\e -> do
                warningM _log $ "Send error: " ++ (show (e :: SomeException))
                disconnect socket)
      isDone <- atomically $ readTVar done
      if isDone
        then return ()
        else sendMessages
      where
          disconnect socket = closeConnectedSocket (connSocket conn) socket

receiver :: Connection -> TVar Bool -> Mailbox Message -> IO ()
receiver conn done mailbox  = do
    socket <- atomically $ connected $ connSocket conn
    receiveSocketMessages socket done (connAddress conn) mailbox

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

closeMessenger :: Messenger -> IO ()
closeMessenger msngr = do
  infoM _log $ "Closing mesenger to " ++ (messengerAddress msngr)
  atomically $ modifyTVar (messengerDone msngr) (\_ -> True)
  cancel $ messengerConnector msngr
  cancel $ messengerSender msngr
  cancel $ messengerReceiver msngr
  connClose $ messengerConnection msngr
  infoM _log $ "Closed messenger to " ++ (messengerAddress msngr)

connector :: Connection -> Mailbox Message -> TVar Bool -> IO ()
connector conn mbox done = do
    infoM _log $ "Connecting on " ++ (show $ connAddress conn)
    atomically $ do
        disconnected $ connSocket conn
        -- maybeMessage <- tryPeekMailbox mbox
        -- case maybeMessage of
        --     Nothing -> retry
        --     Just _ -> return ()
    infoM _log $ "Disconnected on " ++ (show $ connAddress conn)
    isDone <- atomically $ readTVar done
    if isDone then
        return ()
    else do
        let (host,port) = parseSocketAddress $ connAddress conn
        infoM _log $ "Connecting to " ++ (show host) ++ ":" ++ (show port) -- (show address)
        socket <- connConnect conn
        infoM _log $ "Connected to " ++ (show $ connAddress conn)
        -- atomically $ putTMVar (connSocket conn) $ SocketRef 0 socket
        setConnectedSocket (connSocket conn) socket
        connector conn mbox done
