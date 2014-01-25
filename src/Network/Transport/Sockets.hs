{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Transport.Sockets
-- Copyright   :  (c) Phil Hargett 2013
-- License     :  MIT (see LICENSE file)
-- 
-- Maintainer  :  phil@haphazardhouse.net
-- Stability   :  experimental
-- Portability :  non-portable (uses STM)
--
-- Common facilities for socket-based transports, such as UDP and TCP/IP.
--
-----------------------------------------------------------------------------

module Network.Transport.Sockets (

    Bindings,

    SocketTransport(..),

    Connection(..),

    IdentifyMessage(..),

    Messenger(..),
    newMessenger,
    addMessenger,
    deliver,
    closeMessenger,

    dispatcher,
    sender,
    socketSendTo,
    receiver,
    receiveSocketBytes,
    receiveSocketMessage,
    receiveSocketMessages,
    SocketSend,

    parseSocketAddress,
    lookupAddresses,
    lookupAddress

  ) where

-- local imports

import Network.Transport
import Network.Transport.Internal

-- external imports
import Control.Concurrent.Async
import Control.Concurrent.STM

import Control.Exception

import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Serialize
import qualified Data.Set as S
import qualified Data.Text as T

import GHC.Generics

import Network.Socket hiding (recv,socket)
import qualified Network.Socket.ByteString as NSB

import System.Log.Logger

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

_log :: String
_log = "transport.sockets"

type Bindings = TVar (M.Map Name (Mailbox Message))

data SocketTransport = SocketTransport {
  socketMessengers :: TVar (M.Map Address Messenger),
  socketBindings :: Bindings,
  socketConnection :: Address -> IO Connection,
  socketMessenger :: Connection -> Mailbox Message -> IO Messenger,
  socketInbound :: Mailbox Message,
  socketDispatchers :: S.Set (Async ()),
  socketResolver :: Resolver

}

{-|
A connection specializes the use of a transport for a particular
destination.
-}
data Connection = Connection {
  connAddress :: Address,
  connSocket :: TMVar Socket,
  connConnect :: IO Socket,
  connSend :: Socket -> B.ByteString -> IO (),
  connReceive :: Socket -> Int -> IO (Maybe B.ByteString),
  connClose :: IO ()
  }

{-|
A messenger is a facility that actual uses the mechanisms of a transport
(and more specifically, of a connection on a transport) to deliver and receive
messages. The messenger uses 'Mailbox'es internally so that the sending/receiving
happens asynchronously, allowing applications to move on without regard for
when any send / receive action actually completes.
-}
data Messenger = Messenger {
    messengerDone :: TVar Bool,
    messengerOut :: Mailbox Message,
    messengerAddress :: Address,
    messengerSender :: Async (),
    messengerReceiver :: Async (),
    messengerConnection :: Connection
    }

data IdentifyMessage = IdentifyMessage Address deriving (Generic)

instance Serialize IdentifyMessage

{-|
Parse a TCP 'Address' into its respective 'HostName' and 'PortNumber' components, on the
assumption the 'Address' has an identifer in the format @host:port@. If
the port number is missing from the supplied address, it will default to 0.  If the
hostname component is missing from the identifier (e.g., just @:port@), then hostname
is assumed to be @localhost@.
-}
parseSocketAddress :: Address -> (HostName,ServiceName)
parseSocketAddress address = 
  let identifer = T.pack $ address 
      parts = T.splitOn ":" identifer
  in if (length parts) > 1 then
       (host $ T.unpack $ parts !! 0, port $ T.unpack $ parts !! 1)
     else (host $ T.unpack $ parts !! 0, "0")
  where
    host h = if h == "" then
               "localhost"
             else h
    port p = p

lookupAddresses :: (HostName,ServiceName) -> IO [SockAddr]
lookupAddresses hostAndPort = 
    let (host,port) = hostAndPort
        hints = defaultHints { addrFlags = [AI_ADDRCONFIG, AI_CANONNAME, AI_NUMERICSERV] }
    in do 
          addresses <- getAddrInfo (Just hints) (Just host) (Just port)
          return $ map addrAddress addresses

lookupAddress :: (HostName,ServiceName) -> IO SockAddr
lookupAddress hostAndPort = do
    addresses <- lookupAddresses hostAndPort
    return $ addresses !! 0

type SocketSend = Socket -> B.ByteString -> IO ()

instance Show Messenger where
  show msngr = "Messenger(" ++ (show $ messengerAddress msngr) ++ ")"

newMessenger :: Connection -> Mailbox Message -> IO Messenger
newMessenger conn inc = do
  out <- atomically $ newMailbox
  done <- atomically $ newTVar False
  sndr <- async $ sender conn done out
  rcvr <- async $ receiver conn done inc
  return Messenger {
    messengerDone = done,
    messengerOut = out,
    messengerAddress = connAddress conn,
    messengerSender = sndr,
    messengerReceiver = rcvr,
    messengerConnection = conn
    }

addMessenger :: SocketTransport -> Address -> Messenger -> IO ()
addMessenger transport address msngr = do
  msngrs <- atomically $ do
        modifyTVar (socketMessengers transport) $ \msngrs -> M.insert address msngr msngrs
        msngrs <- readTVar (socketMessengers transport)
        return msngrs
  infoM _log $ "Added messenger to " ++ (show address) ++ "; messengers are " ++ (show msngrs)

deliver :: Messenger -> Message -> IO ()
deliver msngr message = atomically $ writeMailbox (messengerOut msngr) message

dispatcher :: TVar (M.Map Name (Mailbox Message)) -> Mailbox Message -> IO ()
dispatcher bindings mbox = dispatchMessages
  where
    dispatchMessages = catchExceptions (do
                                 infoM _log $ "Dispatching messages"
                                 env <- atomically $ readMailbox mbox
                                 dispatchMessage env
                                 dispatchMessages)
                       (\e -> do
                           warningM _log $ "Dispatch error: " ++ (show (e :: SomeException)))
    dispatchMessage env = do
      infoM _log $ "Dispatching message"
      let envelopeOrErr = decode env
      case envelopeOrErr of
        Left err -> do
          errorM _log $ "Error decoding message for dispatch: " ++ err
          return ()
        Right (Envelope destination msg) -> do 
          atomically $ do 
            dests <- readTVar bindings
            let maybeDest = M.lookup destination dests
            case maybeDest of
              Nothing -> return ()
              Just dest -> do 
                writeMailbox dest msg
                return ()

sender :: Connection -> TVar Bool -> Mailbox Message -> IO ()
sender conn done mailbox = sendMessages
  where
    sendMessages = do
      reconnect
      catchExceptions (do
                infoM _log $ "Waiting to send to " ++ (show $ connAddress conn)
                msg <- atomically $ readMailbox mailbox
                infoM _log $ "Sending message to " ++ (show $ connAddress conn)
                connected <- atomically $ tryReadTMVar $ connSocket conn
                case connected of
                  Just socket -> do
                      (connSend conn) socket msg
                  Nothing -> return ()
            )
            (\e -> do
                warningM _log $ "Send error: " ++ (show (e :: SomeException))
                disconnect)
      isDone <- atomically $ readTVar done
      if isDone 
        then return ()
        else sendMessages
    reconnect = do
      -- TODO need a timeout here, in case connecting always fails
      connected <- atomically $ tryReadTMVar $ connSocket conn
      case connected of
        Just _ -> return ()
        Nothing -> do
          let (host,port) = parseSocketAddress $ connAddress conn
          infoM _log $ "Connecting to " ++ (show host) ++ ":" ++ (show port) -- (show address)
          socket <- connConnect conn
          infoM _log $ "Connected to " ++ (show $ connAddress conn)
          atomically $ putTMVar (connSocket conn) socket
    disconnect = do
      connected <- atomically $ tryTakeTMVar $ connSocket conn
      case connected of
        Just socket -> sClose socket
        Nothing -> return ()


socketSendTo :: SocketTransport -> Name -> Message -> IO ()
socketSendTo transport name msg = do
  isLocal <- local
  if isLocal
    then return ()
    else remote
  where
    local = do
      found <- atomically $ do
        bindings <- readTVar $ socketBindings transport
        return $ M.lookup name bindings
      case found of
        Nothing -> return False
        Just mbox -> do
          atomically $ writeMailbox mbox msg
          return True
    remote = do
      Just address <- resolve (socketResolver transport) name
      let env = encode $ Envelope {
            envelopeDestination = name,
            envelopeContents = msg
            }
      amsngr <- atomically $ do
        msngrs <- readTVar $ socketMessengers transport
        return $ M.lookup address msngrs
      case amsngr of
        Nothing -> do
          msngrs <- atomically $ readTVar $ socketMessengers transport
          infoM _log $ "No messenger for " ++ (show address) ++ " in " ++ (show msngrs)
          socketVar <- atomically $ newEmptyTMVar
          newConn <- (socketConnection transport) address
          let conn = newConn {connSocket = socketVar}
          msngr <- (socketMessenger transport) conn (socketInbound transport)
          addMessenger transport address msngr
          deliver msngr env
          return ()
        Just msngr -> deliver msngr env

receiver :: Connection -> TVar Bool -> Mailbox Message -> IO ()
receiver conn done mailbox  = do 
    socket <- atomically $ readTMVar $ connSocket conn
    receiveSocketMessages socket done (connAddress conn) mailbox

receiveSocketMessages :: Socket -> TVar Bool -> Address -> Mailbox Message -> IO ()
receiveSocketMessages sock done addr mailbox = do 
    catchExceptions (do
          infoM _log $ "Waiting to receive on " ++ (show addr)
          maybeMsg <- receiveSocketMessage sock
          infoM _log $ "Received message on " ++ (show addr)
          case maybeMsg of
            Nothing -> do
              sClose sock
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
                else warningM _log $ "Receive error: " ++ (show (e :: SomeException)))

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
  cancel $ messengerSender msngr
  cancel $ messengerReceiver msngr
  connClose $ messengerConnection msngr
  infoM _log $ "Closed messenger to " ++ (messengerAddress msngr)
