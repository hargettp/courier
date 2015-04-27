{-# LANGUAGE DeriveGeneric     #-}
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
  newTCPTransport6,

  lookupAddresses,
  lookupTCPAddress,
  lookupWildcardTCPAddress
  ) where

-- local imports

import Network.Transport
import Network.Transport.Internal
import Network.Transport.Sockets
import Network.Transport.Sockets.Addresses
import Network.Transport.Sockets.Messengers

-- external imports

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception

import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Serialize
import qualified Data.Set as S

import qualified Network.Socket as NS
import qualified Network.Socket.ByteString  as NSB

import System.Log.Logger

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

_log :: String
_log = "transport.tcp"

tcpScheme :: Scheme
tcpScheme = "tcp"

lookupTCPAddress :: Address -> NS.Family -> IO NS.SockAddr
lookupTCPAddress address family = lookupAddress family NS.Stream address

lookupWildcardTCPAddress :: Address -> NS.Family -> IO NS.SockAddr
lookupWildcardTCPAddress address family = lookupWildcardAddress family NS.Stream address

{-|
Create a new 'Transport' suitable for sending messages over TCP/IP (IPv4).  There can
be multiple instances of these 'Transport's: 'Network.Endpoints.Endpoint' using
different instances will still be able to communicate, provided they use
correct TCP/IP addresses (or hostnames) for communication.
-}
newTCPTransport :: Resolver -> IO Transport
newTCPTransport resolver = let family = NS.AF_INET in newSocketTransport resolver tcpScheme (tcpBind family) (newTCPConnection family) newTCPMessenger

{-|
Create a new 'Transport' suitable for sending messages over TCP/IP (IPv4).  There can
be multiple instances of these 'Transport's: 'Network.Endpoints.Endpoint' using
different instances will still be able to communicate, provided they use
correct TCP/IP addresses (or hostnames) for communication.
-}
newTCPTransport6 :: Resolver -> IO Transport
newTCPTransport6 resolver = let family = NS.AF_INET6 in newSocketTransport resolver tcpScheme (tcpBind family) (newTCPConnection family) newTCPMessenger

tcpBind :: NS.Family -> SocketTransport -> SocketBindings -> Mailbox Message -> Name -> IO (Either String Binding)
tcpBind family transport sockets inc name = do
    atomically $ modifyTVar (socketBindings transport) $ \ bindings ->
        M.insert name inc bindings
    Just address <- resolve (socketResolver transport) name
    bindAddress sockets address $ do
        sock <- NS.socket family NS.Stream NS.defaultProtocol
        listener <- do
            infoM _log $ "Binding to address " ++ (show address)
            tcpListen transport family address sock
        return (sock,listener)
    return $ Right Binding {
        bindingName = name,
        unbind = tcpUnbind sockets address
        }

tcpListen :: SocketTransport -> NS.Family -> Address -> NS.Socket -> IO (Async ())
tcpListen transport family address sock = do
            catchExceptions (do
                    NS.setSocketOption sock NS.NoDelay 1
                    NS.setSocketOption sock NS.ReuseAddr 1
                    sockAddr <- lookupWildcardTCPAddress address family
                    NS.bind sock sockAddr
                    NS.listen sock 2048) -- TODO think about a configurable backlog
                (\e -> do
                warningM _log $ "Listen error on port " ++ address ++ ": " ++ (show (e :: SomeException))
                NS.sClose sock)
            async $ tcpAccept transport family address sock

tcpAccept :: SocketTransport -> NS.Family -> Address -> NS.Socket -> IO ()
tcpAccept transport family address sock = do
    infoM _log $ "Listening for connections on " ++ (show address) ++ ": " ++ (show sock)
    (client,clientAddress) <- NS.accept sock
    _ <- async $ tcpDispatch transport family address client clientAddress
    tcpAccept transport family address sock

tcpDispatch :: SocketTransport -> NS.Family -> Address -> NS.Socket -> NS.SockAddr -> IO ()
tcpDispatch transport family address client socketAddress = do
  infoM _log $ "Accepted connection on " ++ (show address)
  identity <- tcpIdentify client socketAddress
  case identity of
        Nothing -> NS.sClose client
        Just (IdentifyMessage clientAddress) -> do
            infoM _log $ "Identified " ++ (show clientAddress)
            newConn <- (newTCPConnection family) clientAddress
            -- atomically $ putTMVar (connSocket newConn) $ SocketRef 0 client
            setConnectedSocket (connSocket newConn) client
            msngr <- newMessenger newConn (socketInbound transport)
            replaceMessenger transport clientAddress msngr

tcpIdentify :: NS.Socket -> NS.SockAddr -> IO (Maybe IdentifyMessage)
tcpIdentify client clientAddress = do
    infoM _log $ "Awaiting identity from " ++ (show clientAddress)
    maybeMsg <- receiveSocketMessage client
    case maybeMsg of
        Nothing -> return Nothing
        Just bytes -> do
            let msg = decode bytes
            case msg of
                Left _ -> return Nothing
                Right message -> return $ Just message

tcpUnbind :: SocketBindings -> Address -> IO ()
tcpUnbind sockets address = do
  infoM _log $ "Unbinding from TCP port " ++ (show address)
  unbindAddress sockets address
  infoM _log $ "Unbound from TCP port " ++ (show address)

newTCPConnection :: NS.Family -> Address -> IO Connection
newTCPConnection family address = do
    var <- atomically $ newSocketVar
    return Connection {
        connAddress = address,
        connSocket = var,
        connConnect = do
            socket <- NS.socket family NS.Stream NS.defaultProtocol
            sockAddr <- lookupTCPAddress address family
            NS.connect socket sockAddr
            infoM _log $ "Initiated socket connection to " ++ (show sockAddr)
            -- NS.connect socket $ NS.addrAddress $ head ipv4Addrs
            -- atomically $ putTMVar sock $ SocketRef 0 socket
            -- setConnectedSocket var socket
            -- infoM _log $ "Initiated socket connection to " ++ (show $ head ipv4Addrs)
            return socket,
        connSend = tcpSend address,
        connReceive = receiveSocketBytes,
        connClose = do
            infoM _log $ "Closing connection to " ++ address
            forceCloseConnectedSocket var
            {-
            state <- atomically $ tryTakeTMVar var
            case socketStateSocket state of
                Just socket -> do
                    infoM _log $ "Closing socket " ++ (show $ socketRefSocket socket) ++ " for " ++ address
                    NS.sClose $ socketRefSocket socket
                    infoM _log $ "Closed socket " ++ (show $ socketRefSocket socket) ++ " for " ++ address
                Nothing -> do
                    infoM _log $ "No socket to close for " ++ address
                    return ()
            -}
            infoM _log $ "Connection to " ++ address ++ " closed"
        }

newTCPMessenger :: Bindings -> Resolver -> Connection -> Mailbox Message -> IO Messenger
newTCPMessenger bindings resolver conn mailbox = do
    msngr <- newMessenger conn mailbox
    tcpIdentifySender bindings resolver msngr
    return msngr

tcpIdentifySender :: Bindings -> Resolver -> Messenger -> IO ()
tcpIdentifySender bindings resolver msngr = do
    bs <- atomically $ readTVar bindings
    boundAddresses <- mapM (resolve resolver) (M.keys bs)
    let uniqueAddresses = S.toList $ S.fromList boundAddresses
    mapM_ (identify msngr) uniqueAddresses
    where
        identify msngr maybeUniqueAddress= do
            case maybeUniqueAddress of
                Nothing -> return()
                Just uniqueAddress -> deliver msngr $ encode $ IdentifyMessage uniqueAddress

tcpSend :: Address -> NS.Socket -> B.ByteString -> IO ()
tcpSend addr sock bs = do
    NSB.sendAll sock $ encode (B.length bs)
    infoM _log $ "Length sent"
    NSB.sendAll sock bs
    infoM _log $ "Message sent to" ++ (show addr)
