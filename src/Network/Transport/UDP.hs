{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Transport.UDP
-- Copyright   :  (c) Phil Hargett 2013
-- License     :  MIT (see LICENSE file)
-- 
-- Maintainer  :  phil@haphazardhouse.net
-- Stability   :  experimental
-- Portability :  non-portable (uses STM)
--
-- UDP transports deliver messages to other 'Network.Endpoints.Endpoint's using UDP/IP.
--
-- Each UDP transport manages socket bindings on behalf of
-- 'Endpoint's, dynamically opening / closing new sockets as needed to deliver
-- messages to other 'Endpoint's using UDP transports.
-----------------------------------------------------------------------------

module Network.Transport.UDP (
  -- newUDPTransport
  UDPTransport(..)
  ) where

-- local imports

import Network.Transport.Sockets
import Network.Transport

-- external imports

import Control.Concurrent.Async
import Control.Concurrent.STM

import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Set as S

import Network.Socket (ServiceName,Socket,socket,Family(..),SocketType(..),defaultProtocol)
import Network.Socket.ByteString(sendAll,recv)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

_log :: String
_log = "transport.udp"

data UDPTransport = UDPTransport {
  udpListeners :: TVar (M.Map ServiceName Socket),
  udpMessengers :: TVar (M.Map Address Messenger),  
  udpBindings :: TVar (M.Map Name Mailbox),
  udpInbound :: Mailbox,
  udpDispatchers :: S.Set (Async ()),
  udpResolver :: Resolver
  }

newUDPConnection :: Address -> IO Connection
newUDPConnection address = do
  sock <- atomically newEmptyTMVar
  return Connection {
    connAddress = address,
    connSocket = sock,
    connConnect = socket AF_INET6 Datagram defaultProtocol,
    connSend = sendAll,
    connReceive = udpReceive
    }
  where
      udpReceive sock byteCount = do
          bytes <- recv sock byteCount
          if B.null bytes
            then return Nothing 
            else return $ Just bytes
