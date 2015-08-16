module TestUtils
    ( newTCPAddress
    , newUDPAddress
    , newTCPAddress6
    , newUDPAddress6
    , timeBound
    , troubleshoot

    ,testTransportEndpointSendReceive
    ,testTransportEndpointSendReceive2SerialClients
    ,testTransportEndpointSendReceive2SerialServers
    )
    where

-- local imports

import Network.Endpoints

-- external imports
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception

import Data.Serialize

import qualified Network.Socket as NS

import System.Log.Logger

import Test.HUnit

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

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
isPrivileged (NS.SockAddrUnix _) = False

timeBound :: Int -> IO () -> IO ()
timeBound delay action = do
    outcome <- race (threadDelay delay) action
    assertBool "Test should not block" $ outcome == Right ()

troubleshoot :: IO () -> IO ()
troubleshoot fn = do
    finally (do
        updateGlobalLogger rootLoggerName (setLevel INFO)
        fn) (updateGlobalLogger rootLoggerName (setLevel WARNING))

testTransportEndpointSendReceive :: IO Transport -> Name -> Name -> Assertion
testTransportEndpointSendReceive transportFactory name1 name2 = timeBound (1 * 1000000 :: Int) $ do
  transport <- transportFactory
  withEndpoint2 transport $ \endpoint1 endpoint2 -> do
    withBinding2 (endpoint1,name1) (endpoint2,name2) $ do
      withConnection endpoint1 name2 $ do
        sendMessage endpoint1 name2 $ encode "hello!"
        msg <- receiveMessage endpoint2
        assertEqual "Received message not same as sent" (Right "hello!") (decode msg)
        return ()

testTransportEndpointSendReceive2SerialServers :: IO Transport -> Name -> Name -> Assertion
testTransportEndpointSendReceive2SerialServers transportFactory name1 name2 = timeBound (1 * 1000000 :: Int) $ do
  transport <- transportFactory
  withEndpoint2 transport $ \endpoint1 endpoint2 -> do
    withBinding endpoint1 name1 $ do
      withBinding endpoint2 name2 $ do
        withConnection endpoint1 name2 $ do
          sendMessage endpoint1 name2 $ encode "hello!"
          msg <- receiveMessage endpoint2
          assertEqual "Received message not same as sent" (Right "hello!") (decode msg)
          return ()
      withBinding endpoint2 name2 $ do
        withConnection endpoint1 name2 $ do
          sendMessage endpoint1 name2 $ encode "hello!"
          msg <- receiveMessage endpoint2
          assertEqual "Received message not same as sent" (Right "hello!") (decode msg)
          return ()

testTransportEndpointSendReceive2SerialClients :: IO Transport -> Name -> Name -> Assertion
testTransportEndpointSendReceive2SerialClients transportFactory name1 name2 = timeBound (1 * 1000000 :: Int) $ do
  transport <- transportFactory
  withEndpoint2 transport $ \endpoint1 endpoint2 -> do
    withBinding endpoint2 name2 $ do
      withBinding endpoint1 name1 $ do
        withConnection endpoint1 name2 $ do
          sendMessage endpoint1 name2 $ encode "hello!"
          msg <- receiveMessage endpoint2
          assertEqual "Received message not same as sent" (Right "hello!") (decode msg)
          return ()
      withBinding endpoint1 name1 $ do
        withConnection endpoint1 name2 $ do
          sendMessage endpoint1 name2 $ encode "hello!"
          msg <- receiveMessage endpoint2
          assertEqual "Received message not same as sent" (Right "hello!") (decode msg)
          return ()
