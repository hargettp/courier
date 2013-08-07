module TestTCP (tests) where

-- local imports

import Network.Endpoints
import Network.Transport.TCP

-- external imports

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM

import Data.Serialize

import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit
-- import Test.Framework.Providers.QuickCheck2

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

tests :: [Test.Framework.Test]
tests = 
  [
    testCase "tcp-endpoints+transport" testEndpointTransport,
    testCase "tcp-unbind" testEndpointBindUnbind,
    testCase "tcp-sendReceive" testEndpointSendReceive
    -- testCase "tcp-transport" testTCPTransport
  ] 
  
testEndpointTransport :: Assertion  
testEndpointTransport = do  
  transport <- newTCPTransport
  _ <- newEndpoint [transport]
  return ()
  
testEndpointBindUnbind :: Assertion
testEndpointBindUnbind = do  
  transport <- newTCPTransport
  endpoint <- newEndpoint [transport]
  let address = newTCPAddress "localhost:2000"
  Right () <- bindEndpoint endpoint address
  unbound <- unbindEndpoint endpoint address
  case unbound of
    Left err -> assertFailure $ "Unbind failed: " ++ err
    Right () -> assertBool "Unbind succeeded" True
  return ()
  
testEndpointSendReceive :: Assertion  
testEndpointSendReceive = do
  transport1 <- newTCPTransport
  transport2 <- newTCPTransport
  endpoint1 <- newEndpoint [transport1]
  endpoint2 <- newEndpoint [transport2]
  let address1 = newTCPAddress "localhost:2000"
      address2 = newTCPAddress "localhost:2001"
  Right () <- bindEndpoint endpoint1 address1
  Right () <- bindEndpoint endpoint2 address2
  threadDelay $ 1 * 1000000
  _ <- sendMessage endpoint1 address2 $ encode "hello!"
  msgVar <- atomically $ newTVar Nothing
  withAsync (do
                msg <- receiveMessage endpoint2    
                atomically $ writeTVar msgVar $ Just msg)
    (\_ -> do 
        threadDelay (2 * 1000000)
        Just msg <- atomically $ readTVar msgVar
        assertEqual "Received message not same as sent" (Right "hello!") (decode msg))
  return ()
  