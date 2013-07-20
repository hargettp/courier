module TestMemory (tests) where

-- local imports

import Network.Endpoints
import Network.Transport.Memory

-- external imports

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
    testCase "endpoints+transport" testEndpointTransport,
    testCase "bind" testEndpointBind,
    testCase "unbind" testEndpointBindUnbind,
    testCase "sendReceive" testEndpointSendReceive,
    testCase "memory" testMemoryTransport
  ] 
  
testEndpointTransport :: Assertion  
testEndpointTransport = do  
  transport <- newMemoryTransport
  _ <- newEndpoint [transport]
  return ()
  
testEndpointBind :: Assertion
testEndpointBind = do  
  transport <- newMemoryTransport
  endpoint <- newEndpoint [transport]
  let address = newMemoryAddress "endpoint1"
  Right () <- bindEndpoint endpoint address
  return ()

testEndpointBindUnbind :: Assertion
testEndpointBindUnbind = do  
  transport <- newMemoryTransport
  endpoint <- newEndpoint [transport]
  let address = newMemoryAddress "endpoint1"
  Right () <- bindEndpoint endpoint address
  unbound <- unbindEndpoint endpoint address
  case unbound of
    Left err -> assertFailure $ "Unbind failed: " ++ err
    Right () -> assertBool "Unbind succeeded" True
  return ()
  
testEndpointSendReceive :: Assertion  
testEndpointSendReceive = do
  transport <- newMemoryTransport
  endpoint1 <- newEndpoint [transport]
  endpoint2 <- newEndpoint [transport]
  let address1 = newMemoryAddress "endpoint1"
      address2 = newMemoryAddress "endpoint2"
  Right () <- bindEndpoint endpoint1 address1
  Right () <- bindEndpoint endpoint2 address2
  _ <- sendMessage endpoint1 address2 $ encode "hello!"
  msg <- receiveMessage endpoint2    
  assertEqual "Received message not same as sent" (Right "hello!") (decode msg)
  return ()
  
-- Memory tests
  
testMemoryTransport :: Assertion
testMemoryTransport = do
  _ <- newMemoryTransport
  return ()

