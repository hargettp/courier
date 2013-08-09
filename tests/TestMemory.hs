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
    testCase "mem-endpoints+transport" testEndpointTransport,
    testCase "mem-bind" testEndpointBind,
    testCase "mem-unbind" testEndpointBindUnbind,
    testCase "mem-sendReceive" testEndpointSendReceive,
    testCase "mem-transport" testMemoryTransport
  ] 
  
testEndpointTransport :: Assertion  
testEndpointTransport = do  
  transport <- newMemoryTransport
  _ <- newEndpoint [transport]
  return ()
  
testEndpointBind :: Assertion
testEndpointBind = do  
  let name1 = "endpoint1"
  transport <- newMemoryTransport
  endpoint <- newEndpoint [transport]
  Right () <- bindEndpoint endpoint name1
  return ()

testEndpointBindUnbind :: Assertion
testEndpointBindUnbind = do  
  let name1 = "endpoint1"
  transport <- newMemoryTransport
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
  transport <- newMemoryTransport
  endpoint1 <- newEndpoint [transport]
  endpoint2 <- newEndpoint [transport]
  Right () <- bindEndpoint endpoint1 name1
  Right () <- bindEndpoint endpoint2 name2
  _ <- sendMessage endpoint1 name2 $ encode "hello!"
  msg <- receiveMessage endpoint2    
  assertEqual "Received message not same as sent" (Right "hello!") (decode msg)
  return ()
  
-- Memory tests
  
testMemoryTransport :: Assertion
testMemoryTransport = do
  _ <- newMemoryTransport
  return ()

