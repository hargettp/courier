module TestUtils
    (
      newTCPAddress
    , newUDPAddress
    , newTCPAddress6
    , newUDPAddress6
    , timeBound
    , troubleshoot

    ,isIPv6Available
    )
    where

-- local imports

import Network.Transport.Sockets.TCP

-- external imports
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception

import qualified Network.Socket as NS

import System.Log.Logger

import Test.HUnit

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

newTCPAddress :: IO String
newTCPAddress = do
  NS.SockAddrInet p _ <- availablePort NS.AF_INET NS.Stream
  return $ "localhost:" ++ show p

newUDPAddress :: IO String
newUDPAddress = do
  NS.SockAddrInet p _ <- availablePort NS.AF_INET NS.Datagram
  return $ "localhost:" ++ show p

newTCPAddress6 :: IO String
newTCPAddress6 = do
  NS.SockAddrInet6 p _ _ _ <- availablePort NS.AF_INET6 NS.Stream
  return $ "localhost:" ++ show p

newUDPAddress6 :: IO String
newUDPAddress6 = do
  NS.SockAddrInet6 p _ _ _ <- availablePort NS.AF_INET6 NS.Datagram
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

isIPv6Available :: IO Bool
isIPv6Available = do
    addresses <- tcpSocketResolver6 (Name "localhost:1")
    case addresses of
        [] -> return False
        _ -> return True

isPrivileged :: NS.SockAddr -> Bool
isPrivileged (NS.SockAddrInet p _) = p < 1025
isPrivileged (NS.SockAddrInet6 p _ _ _) = p < 1025
isPrivileged _ = False

timeBound :: Int -> IO () -> IO ()
timeBound delay action = do
    outcome <- race (threadDelay delay) action
    assertBool "Test should not block" $ outcome == Right ()

troubleshoot :: IO () -> IO ()
troubleshoot fn = do
    finally (do
        updateGlobalLogger rootLoggerName (setLevel INFO)
        fn) (updateGlobalLogger rootLoggerName (setLevel WARNING))
