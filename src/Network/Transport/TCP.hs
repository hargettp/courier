{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Transport.TCP
-- Copyright   :  (c) Phil Hargett 2013
-- License     :  MIT (see LICENSE file)
-- 
-- Maintainer  :  phil@haphazardhouse.net
-- Stability   :  experimental
-- Portability :  non-portable (uses STM)
--
-- TCP transports deliver messages to other 'Network.Endpoints.Endpoint's using TCP/IP.
--
-- Each TCP transport manages both socket bindings and connections on behalf of
-- 'Endpoint's, dynamically opening / closing new sockets as needed to deliver
-- messages to other 'Endpoint's using TCP transports.
-----------------------------------------------------------------------------

module Network.Transport.TCP (
  newTCPTransport,
  newTCPAddress
  ) where

-- local imports

import Network.Transport

-- external imports

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (bracketOnError)

import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Serialize

import Network hiding (sendTo)
import Network.BSD (getHostByName,hostAddress,getProtocolNumber)
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as S
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

tcpScheme :: Scheme
tcpScheme = "tcp"

data TCPTransport = TCPTransport {
  boundSockets :: TVar (M.Map Address N.Socket),
  connectedSockets :: SocketMap
  }
                    
type SocketMap = TVar (M.Map Address TCPSocket)

newSocketMap :: IO SocketMap
newSocketMap = atomically $ newTVar M.empty

findSocket :: SocketMap -> Address -> IO (Maybe TCPSocket)
findSocket sockets address = do
  smap <- atomically $ readTVar sockets
  return $ M.lookup address smap  
  
-- | insert the socket into the map only if empty, returning the socket
-- that is in the map with a boolean; the bool is true if the socket was 
-- newly inserted, false if no insert was made.  This implies that if true,
-- the returned socket is a new one, and if false, the returned socket
-- is a pre-existing one
putSocketIfAbsent :: SocketMap -> Address -> TCPSocket -> IO (TCPSocket,Bool)
putSocketIfAbsent sockets address socket = atomically $ do
  smap <- readTVar sockets
  let maybeExisting = M.lookup address smap
  case maybeExisting of
    Just existing -> return (existing,False)
    Nothing -> do
      modifyTVar sockets (\socks -> M.insert address socket socks)
      return (socket,True)
  
{-|
Create an 'Address' suitable for use with TCP 'Transport's
-}
newTCPAddress :: String -> Address
newTCPAddress address = Address {
  addressScheme = tcpScheme,
  addressIdentifier = address
  }
                        
{-|
Parse a TCP 'Address' into its respective 'HostName' and 'PortNumber' components, on the
assumption the 'Address' has an identifer in the format @host:port@. If
the port number is missing from the supplied address, it will default to 0.  If the
hostname component is missing from the identifier (e.g., just @:port@), then hostname
is assumed to be @localhost@.
-}
parseTCPAddress :: Address -> (HostName,PortNumber)
parseTCPAddress address = 
  if tcpHandles address then
    let identifer = T.pack $ addressIdentifier address 
        parts = T.splitOn ":" identifer
    in if (length parts) > 1 then
         (host $ T.unpack $ parts !! 0, port $ T.unpack $ parts !! 1)
       else (host $ T.unpack $ parts !! 0, 0)
    else error $ "Not a TCP addres: " ++ (show address)
  where
    host h = if h == "" then
               "localhost"
             else h
    port p = fromIntegral (read p :: Int)

{-|
Create a new 'Transport' suitable for sending messages over TCP/IP.  There can
be multiple instances of these 'Transport's: 'Network.Endpoints.Endpoint' using
different instances will still be able to communicate, provided they use
correct TCP/IP addresses (or hostnames) for communication.
-}
newTCPTransport :: IO Transport
newTCPTransport = do 
  bound <- atomically $ newTVar M.empty
  connected <- newSocketMap
  let transport = TCPTransport {
        boundSockets = bound,
        connectedSockets = connected
        }
  return Transport {
      scheme = tcpScheme,
      handles = tcpHandles,
      bind = tcpBind transport,
      sendTo = tcpSendTo transport,
      shutdown = tcpShutdown transport
      }

tcpHandles :: Address -> Bool
tcpHandles address = tcpScheme == (addressScheme address)

tcpBind :: TCPTransport -> Mailbox -> Address -> IO (Either String Binding)
tcpBind transport incoming address = do 
  listener <- async listenForConenctions
  return $ Right Binding {
    bindingAddress = address,
    unbind = tcpUnbind transport address listener
    }
  where
    listenForConenctions = do
      socket <- tcpListen
      acceptConnections socket
    acceptConnections socket = do
      _ <- tcpAccept socket
      acceptConnections socket
    tcpListen = do
      let (_,port) = parseTCPAddress address
      proto <- getProtocolNumber "tcp"
      bracketOnError
        (N.socket N.AF_INET N.Stream proto)
        (sClose)
        (\sock -> do
            N.setSocketOption sock N.ReuseAddr 1
            N.bindSocket sock (N.SockAddrInet port N.iNADDR_ANY)
            N.listen sock 1024
            return sock)
    tcpAccept socket = do
      (client,_) <- N.accept socket
      outgoing <- newMailbox
      return $ newTCPSocket client incoming outgoing

tcpSendTo :: TCPTransport -> Address -> B.ByteString -> IO ()
tcpSendTo transport address msg = do
  -- yes, we're using an empty mailbox for now; connections aren't really
  -- bidirectional at the moment
  incoming <- newMailbox
  socket <- tcpConnect transport address incoming
  atomically $ writeTQueue (socketOutgoing socket) msg
  return ()
  
-- | Either a connect a new socket, or find an existing socket to reuse
tcpConnect :: TCPTransport -> Address -> Mailbox -> IO TCPSocket
tcpConnect transport address incoming = do
  maybeExisting <- findSocket (connectedSockets transport) address
  case maybeExisting of
    Just connection -> return connection
    Nothing -> do
      connection <- connect
      (existing,new) <- putSocketIfAbsent (connectedSockets transport) address connection
      if new 
        then return existing
        else do
             closeTCPSocket connection
             return existing
  where 
    connect = do
      let (host,port) = parseTCPAddress address
      proto <- getProtocolNumber "tcp"
      outgoing <- newMailbox
      socket <- bracketOnError
            (N.socket N.AF_INET N.Stream proto)
            (N.sClose)  -- only done if there's an error
            (\sock -> do
                he <- getHostByName host
                N.connect sock (N.SockAddrInet port (hostAddress he))
                return sock)
      newTCPSocket socket incoming outgoing

tcpUnbind :: TCPTransport -> Address -> Async ()-> IO ()
tcpUnbind transport address listener = do
  cancel listener
  -- maybeSocket <- removeSocket (boundSockets transport) address
  maybeSocket <- atomically $ do 
    sockets <- readTVar (boundSockets transport)
    let maybeSocket = M.lookup address sockets
    modifyTVar (boundSockets transport) (\smap -> M.delete address smap)
    return maybeSocket
  case maybeSocket of
    Nothing -> return ()
    Just socket -> N.sClose socket

tcpShutdown :: TCPTransport -> IO ()
tcpShutdown transport = do
  bound <- atomically $ readTVar $ boundSockets transport
  connected <- atomically $ readTVar $ connectedSockets transport
  mapM_ N.sClose $ M.elems bound
  mapM_ closeTCPSocket $ M.elems connected
  return ()

data TCPSocket = TCPSocket {
  tcpSocket :: Socket,
  socketOutgoing :: Mailbox,
  socketMessenger :: Async ()
  }
                 
newTCPSocket :: Socket -> Mailbox -> Mailbox -> IO TCPSocket                 
newTCPSocket socket incoming outgoing = do
  msgr <- messenger socket incoming outgoing
  return TCPSocket {
    tcpSocket = socket,
    socketOutgoing = outgoing,
    socketMessenger = msgr
    }
    
closeTCPSocket :: TCPSocket -> IO ()
closeTCPSocket socket = do
  sClose (tcpSocket socket)
  cancel $ socketMessenger socket
    
{-|    
A messenger spawns separate 'Async's to send and receive messages
over the socket, as well a separate 'Async' for itsel.  If either the sender or 
receiver hit an exception, or if the messenger''s 'Async' hits an exception,
then all 3 will be 'cancel'led.
-}
messenger :: Socket -> Mailbox -> Mailbox -> IO (Async ())    
messenger socket incoming outgoing = async $ do
  _ <- concurrently (sender socket outgoing) (receiver socket incoming)
  return ()
                 
sender :: Socket -> Mailbox -> IO ()
sender socket mailbox = send
  where
    send = do
      msg <- atomically $ readTQueue mailbox
      S.sendAll socket $ encode (B.length msg)
      S.sendAll socket msg
      send

receiver :: Socket -> Mailbox -> IO ()
receiver socket mailbox  = receive
  where
    receive = do
      len <- S.recv socket 4 -- TODO must figure out what defines length of an integer in bytes 
      msg <- S.recv socket $ msgLength (decode len)
      atomically $ writeTQueue mailbox msg
      receive
    msgLength (Right size) = size
    msgLength (Left err) = error err