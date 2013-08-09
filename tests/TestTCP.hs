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
    testCase "tcp-bind-unbind" testEndpointBindUnbind,
    testCase "tcp-sendReceive" testEndpointSendReceive
  ] 
  
testEndpointTransport :: Assertion  
testEndpointTransport = do  
  let name1 = "endpoint1"
      name2 = "endpoint2"
  let resolver = resolverFromList [(name1,newTCPAddress "localhost:2000"),
                               (name2,newTCPAddress "localhost:2001")]
  transport <- newTCPTransport resolver
  _ <- newEndpoint [transport]
  return ()
  
testEndpointBindUnbind :: Assertion
testEndpointBindUnbind = do  
  let name1 = "endpoint1"
      name2 = "endpoint2"
  let resolver = resolverFromList [(name1,newTCPAddress "localhost:2000"),
                               (name2,newTCPAddress "localhost:2001")]
  transport <- newTCPTransport resolver
  endpoint <- newEndpoint [transport]
  Right () <- bindEndpoint endpoint name1
  unbound <- unbindEndpoint endpoint name1
  case unbound of
    Left err -> assertFailure $ "Unbind failed: " ++ err
    Right () -> assertBool "Unbind succeeded" True
  return ()
  
testEndpointSendReceive :: Assertion
testEndpointSendReceive = do
  let name1 = "endpoint1"
      name2 = "endpoint2"
  let resolver = resolverFromList [(name1,newTCPAddress "localhost:2000"),
                               (name2,newTCPAddress "localhost:2001")]
  transport1 <- newTCPTransport resolver
  transport2 <- newTCPTransport resolver
  endpoint1 <- newEndpoint [transport1]
  endpoint2 <- newEndpoint [transport2]
  Right () <- bindEndpoint endpoint1 name1
  Right () <- bindEndpoint endpoint2 name2
  threadDelay $ 1 * 1000000
  _ <- sendMessage endpoint1 name2 $ encode "hello!"
  msgVar <- atomically $ newTVar Nothing
  withAsync (do
                msg <- receiveMessage endpoint2    
                atomically $ writeTVar msgVar $ Just msg)
    (\_ -> do 
        threadDelay (2 * 1000000)
        Just msg <- atomically $ readTVar msgVar
        assertEqual "Received message not same as sent" (Right "hello!") (decode msg))
  return ()
  