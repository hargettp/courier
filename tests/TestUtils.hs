module TestUtils
    ( newTCPAddress
    , newUDPAddress
    , newTCPAddress6
    , newUDPAddress6)
    where

import Control.Exception
import qualified Network.Socket as NS

newTCPAddress :: IO String
newTCPAddress = do
  NS.SockAddrInet (NS.PortNum p) _ <- availablePort NS.AF_INET NS.Stream
  return $ "localhost:" ++ show p

newUDPAddress :: IO String
newUDPAddress = do
  NS.SockAddrInet (NS.PortNum p) _ <- availablePort NS.AF_INET NS.Datagram
  return $ "localhost:" ++ show p

newTCPAddress6 :: IO String
newTCPAddress6 = do
  NS.SockAddrInet6 (NS.PortNum p) _ _ _ <- availablePort NS.AF_INET6 NS.Stream
  return $ "localhost:" ++ show p

newUDPAddress6 :: IO String
newUDPAddress6 = do
  NS.SockAddrInet6 (NS.PortNum p) _ _ _ <- availablePort NS.AF_INET6 NS.Datagram
  return $ "localhost:" ++ show p

availablePort     :: NS.Family -> NS.SocketType -> IO NS.SockAddr
availablePort f t = do
  let hints = NS.defaultHints { NS.addrFamily = f
                              , NS.addrSocketType = t
                              , NS.addrFlags = [ NS.AI_PASSIVE ]
                              , NS.addrProtocol = NS.defaultProtocol }
  addrs <- NS.getAddrInfo (Just hints) Nothing (Just "0")
  let a = head addrs
  bracket
    (NS.socket (NS.addrFamily a) (NS.addrSocketType a) (NS.addrProtocol a))
    NS.close
    (\s -> do
       NS.bindSocket s (NS.addrAddress a)
       addr <- NS.getSocketName s
       if isPrivileged addr
         then availablePort f t
         else return addr
    )

isPrivileged :: NS.SockAddr -> Bool
isPrivileged (NS.SockAddrInet (NS.PortNum p) _) = p < 1025
isPrivileged (NS.SockAddrInet6 (NS.PortNum p) _ _ _) = p < 1025
