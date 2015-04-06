{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Transport.Sockets.Addresses
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

module Network.Transport.Sockets.Addresses (
    parseSocketAddress,
    lookupAddresses,
    lookupAddress,
    lookupWildcardAddress

) where

-- local imports
import Network.Endpoints

-- external imports
import qualified Data.Text as T

import Network.Socket hiding (bind, recv, sendTo,shutdown, socket)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
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

