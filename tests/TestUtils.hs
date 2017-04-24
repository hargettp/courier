module TestUtils
    (
    withNewEndpoint,
    withNewEndpoint2,
    withNewEndpoint3,
    withNewEndpoint4,

    newTCPAddress,
    newUDPAddress,
    newTCPAddress6,
    newUDPAddress6,
    isIPv6Available,

    timeBound,
    troubleshoot,
    shareTransport

    )
  where

-- local imports

import Network.Endpoints
import Network.Transport.Sockets.TCP

-- external imports
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception

import qualified Network.Socket as NS

import System.Log.Logger

import Test.HUnit

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

withNewEndpoint :: Transport -> (Endpoint -> IO ()) -> IO ()
withNewEndpoint transport fn = do
  endpoint <- newEndpoint
  withEndpoint transport endpoint $ fn endpoint

withNewEndpoint2 :: Transport -> (Endpoint -> Endpoint -> IO ()) -> IO ()
withNewEndpoint2 transport fn = do
  withNewEndpoint transport $ \endpoint1 ->
    withNewEndpoint transport $ \endpoint2 ->
      fn endpoint1 endpoint2

withNewEndpoint3 :: Transport -> (Endpoint -> Endpoint -> Endpoint -> IO ()) -> IO ()
withNewEndpoint3 transport fn = withNewEndpoint transport $ \endpoint1 ->
  withNewEndpoint2 transport $ \endpoint2 endpoint3 ->
    fn endpoint1 endpoint2 endpoint3

withNewEndpoint4 :: Transport -> (Endpoint -> Endpoint -> Endpoint -> Endpoint -> IO ()) -> IO ()
withNewEndpoint4 transport fn = withNewEndpoint2 transport $ \endpoint1 endpoint2 ->
  withNewEndpoint2 transport $ \endpoint3 endpoint4 ->
    fn endpoint1 endpoint2 endpoint3 endpoint4

newTCPAddress :: IO Name
newTCPAddress = do
  NS.SockAddrInet p _ <- availablePort NS.AF_INET NS.Stream
  return $ Name $ "localhost:" ++ show p

newUDPAddress :: IO Name
newUDPAddress = do
  NS.SockAddrInet p _ <- availablePort NS.AF_INET NS.Datagram
  return $ Name $ "localhost:" ++ show p

newTCPAddress6 :: IO Name
newTCPAddress6 = do
  NS.SockAddrInet6 p _ _ _ <- availablePort NS.AF_INET6 NS.Stream
  return $ Name $ "localhost:" ++ show p

newUDPAddress6 :: IO Name
newUDPAddress6 = do
  NS.SockAddrInet6 p _ _ _ <- availablePort NS.AF_INET6 NS.Datagram
  return $ Name $ "localhost:" ++ show p

availablePort :: NS.Family -> NS.SocketType -> IO NS.SockAddr
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

shareTransport :: IO Transport -> IO (IO Transport)
shareTransport transportFactory  = do
  transport <- transportFactory
  vTransport <- atomically $ newTVar transport
  return $ atomically $ readTVar vTransport
