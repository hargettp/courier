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

    newSocketBindings,
    SocketBindings,
    bindAddress,
    unbindAddress,
    closeBindings,

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
    lookupAddress,
    lookupWildcardAddress

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

import Network.Socket hiding (recv,socket,bind,sendTo,shutdown)
import qualified Network.Socket.ByteString as NSB

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
    socketCount :: TVar Int,
    socketSocket :: TMVar Socket,
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

lookupAddresses :: Family -> SocketType -> Address -> IO [SockAddr]
lookupAddresses family socketType address =
    let (host,port) = parseSocketAddress address
        hints = defaultHints { addrFlags = [AI_ADDRCONFIG, AI_CANONNAME, AI_NUMERICSERV] }
    in do
          addresses <- getAddrInfo (Just hints) (Just host) (Just port)
          return $ map addrAddress $ filter (\addrInfo -> addrFamily addrInfo == family && addrSocketType addrInfo == socketType) addresses

lookupAddress :: Family -> SocketType -> Address -> IO SockAddr
lookupAddress family socketType address = do
    addresses <- lookupAddresses family socketType address
    return $ addresses !! 0

lookupWildcardAddress :: Family -> SocketType -> Address -> IO SockAddr
lookupWildcardAddress family socketType address = do
    sockAddr <- lookupAddress family socketType address
    case sockAddr of
        SockAddrInet port _ -> return $ SockAddrInet port iNADDR_ANY
        SockAddrInet6 port flow _ scope -> return $ SockAddrInet6 port flow iN6ADDR_ANY scope
        _ -> return sockAddr

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
      infoM _log $ "Reconnecting to " ++ (show $ connAddress conn)
      connected <- atomically $ tryReadTMVar $ connSocket conn
      case connected of
        Just _ -> do
          infoM _log $ "Reconnected to " ++ (show $ connAddress conn)
          return ()
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
  cancel $ messengerSender msngr
  cancel $ messengerReceiver msngr
  connClose $ messengerConnection msngr
  infoM _log $ "Closed messenger to " ++ (messengerAddress msngr)

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
