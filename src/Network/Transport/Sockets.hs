{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

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

    newSocketBindings,
    SocketBindings,
    bindAddress,
    unbindAddress,
    closeBindings,

    SocketRef(..),
    SocketVar,
    newSocketVar,
    setConnectedSocket,
    closeConnectedSocket,
    forceCloseConnectedSocket,
    connected,
    disconnected,

    SocketTransport(..),
    SocketConnectionFactory,
    SocketMessengerFactory,
    SocketBindingFactory,
    newSocketTransport,

    Connection(..),

    IdentifyMessage(..),

    Messenger(..),
    newMessenger,
    addMessenger,
    replaceMessenger,
    deliver,
    closeMessenger,

    dispatcher,
    socketSendTo,
    SocketSend,

  ) where

-- local imports

import Network.Transport
import Network.Transport.Internal

import Network.Transport.Sockets.Connections
import Network.Transport.Sockets.Messengers

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

import Network.Socket hiding (bind, recv, sendTo,shutdown, socket)
import qualified Network.Socket.ByteString  as NSB

import System.Log.Logger

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

_log :: String
_log = "transport.sockets"

type SocketConnectionFactory = Address -> IO Connection
type SocketMessengerFactory = Bindings -> Resolver -> Connection -> Mailbox Message -> IO Messenger
type SocketBindingFactory = SocketTransport -> SocketBindings -> Mailbox Message -> Name -> IO (Either String Binding)

newSocketTransport :: Resolver -> Scheme -> SocketBindingFactory -> SocketConnectionFactory -> SocketMessengerFactory -> IO Transport
newSocketTransport resolver socketScheme binder connectionFactory messengerFactory = do
  messengers <- atomically $ newTVar M.empty
  bindings <- atomically $ newTVar M.empty
  sockets <- newSocketBindings
  inbound <- atomically $ newMailbox
  dispatch <- async $ dispatcher bindings inbound
  let transport = SocketTransport {
        socketMessengers = messengers,
        socketBindings = bindings,
        socketConnection = connectionFactory,
        socketMessenger = messengerFactory bindings resolver,
        socketInbound = inbound,
        socketDispatchers = S.fromList [dispatch],
        socketResolver = resolver
        }
  return Transport {
      scheme = socketScheme,
      handles = socketTransportHandles transport,
      bind = binder transport sockets,
      sendTo = socketSendTo transport,
      shutdown = socketTransportShutdown transport sockets
      }

type Bindings = TVar (M.Map Name (Mailbox Message))

data SocketBinding = SocketBinding {
    socketCount    :: TVar Int,
    socketSocket   :: TMVar Socket,
    socketListener :: TMVar (Async ())
}

socketTransportHandles :: SocketTransport -> Name -> IO Bool
socketTransportHandles transport name = do
  resolved <- resolve (socketResolver transport) name
  return $ isJust resolved
  where
    isJust (Just _) = True
    isJust _ = False

type SocketBindings = TVar (M.Map Address SocketBinding)

newSocketBindings :: IO SocketBindings
newSocketBindings = atomically $ newTVar M.empty

bindAddress :: SocketBindings -> Address -> IO (Socket,Async ()) -> IO ()
bindAddress bindings address factory = do
    (count,binding) <- atomically $ do
        bmap <- readTVar bindings
        binding <- case M.lookup address bmap of
            Nothing -> do
                count <- newTVar 1
                listener <- newEmptyTMVar
                sock <- newEmptyTMVar
                let binding = SocketBinding {
                    socketCount = count,
                    socketListener = listener,
                    socketSocket = sock
                    }
                modifyTVar bindings $ \bs -> M.insert address binding bs
                return binding
            Just binding -> do
                modifyTVar (socketCount binding) $ \c -> c + 1
                return binding
        count <- readTVar $ socketCount binding
        return (count,binding)
    if count == 1
        then do
            infoM _log $ "Opening binding for " ++ (show address)
            (sock,listener) <- factory
            infoM _log $ "Opened binding for " ++ (show address)
            atomically $ do
                putTMVar (socketSocket binding) sock
                putTMVar (socketListener binding) listener
            return ()
        else return ()

unbindAddress :: SocketBindings -> Address -> IO ()
unbindAddress bindings address = do
    (count,maybeBinding) <- atomically $ do
        bmap <- readTVar bindings
        case M.lookup address bmap of
            Nothing -> return (0,Nothing)
            Just binding -> do
                modifyTVar (socketCount binding) $ \count -> count - 1
                count <- readTVar (socketCount binding)
                if count == 0
                    then do
                        modifyTVar bindings $ \bm -> M.delete address bm
                        sock <- takeTMVar $ socketSocket binding
                        listener <- takeTMVar $ socketListener binding
                        return (0,Just (sock,listener))
                    else return (count,Nothing)
    case maybeBinding of
        -- no binding to shutdown; can just return
        Nothing -> do
            infoM _log $ "No binding to shutdown for " ++ (show address) ++ "; count is " ++ (show count)
            return ()
        -- we're the last, so close the binding
        Just (sock,listener) -> do
            infoM _log $ "Closing binding for " ++ (show address) ++ "; count is " ++ (show count)
            cancel listener
            sClose sock
            infoM _log $ "Closed binding for " ++ (show address)

data SocketTransport = SocketTransport {
  socketMessengers  :: TVar (M.Map Address Messenger),
  socketBindings    :: Bindings,
  socketConnection  :: Address -> IO Connection,
  socketMessenger   :: Connection -> Mailbox Message -> IO Messenger,
  socketInbound     :: Mailbox Message,
  socketDispatchers :: S.Set (Async ()),
  socketResolver    :: Resolver
}

data IdentifyMessage = IdentifyMessage Address deriving (Generic)

instance Serialize IdentifyMessage

type SocketSend = Socket -> B.ByteString -> IO ()

instance Show Messenger where
  show msngr = "Messenger(" ++ (show $ messengerAddress msngr) ++ ")"

addMessenger :: SocketTransport -> Address -> Messenger -> IO ()
addMessenger transport address msngr = do
  msngrs <- atomically $ do
        modifyTVar (socketMessengers transport) $ \msngrs -> M.insert address msngr msngrs
        msngrs <- readTVar (socketMessengers transport)
        return msngrs
  infoM _log $ "Added messenger to " ++ (show address) ++ "; messengers are " ++ (show msngrs)

replaceMessenger :: SocketTransport -> Address -> Messenger -> IO ()
replaceMessenger transport address msngr = do
    found <- atomically $ do
        msngrs <- readTVar $ socketMessengers transport
        return $ M.lookup address msngrs
    case found of
        Just _ -> do
              infoM _log $ "Already have messenger for " ++ (show address)
              closeMessenger msngr
        Nothing -> do
            addMessenger transport address msngr

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

closeBindings :: SocketBindings -> IO ()
closeBindings sockets = do
  infoM _log $ "Closing bindings"
  bindings <- atomically $ readTVar sockets
  mapM_ (unbindAddress sockets)  $ M.keys bindings
  infoM _log $ "Closed bindings"

socketTransportShutdown :: SocketTransport -> SocketBindings -> IO ()
socketTransportShutdown transport sockets = do
  closeBindings sockets
  infoM _log $ "Closing messengers"
  msngrs <- atomically $ readTVar $ socketMessengers transport
  mapM_ closeMessenger $ M.elems msngrs
  infoM _log $ "Closing dispatcher"
  mapM_ cancel $ S.toList $ socketDispatchers transport
  mapM_ wait $ S.toList $ socketDispatchers transport
