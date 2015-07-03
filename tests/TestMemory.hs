module TestMemory (tests) where

-- local imports

import Network.Endpoints
import Network.Transport.Memory

-- external imports

import Control.Exception

import Data.Serialize

import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit
-- import Test.Framework.Providers.QuickCheck2

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

tests :: [Test.Framework.Test]
tests = [
    testCase "mem-endpoints+transport" testEndpointTransport,
    {-
    testCase "mem-bind" testEndpointBind,
    testCase "mem-unbind" testEndpointBindUnbind,
    -}
    testCase "mem-sendReceive" testEndpointSendReceive,
    testCase "mem-transport" testMemoryTransport
  ]

testEndpointTransport :: Assertion
testEndpointTransport = do
  transport <- newMemoryTransport
  withTransport transport $ \_ -> return ()

{-
testEndpointBind :: Assertion
testEndpointBind = do
  let name1 = Name "endpoint1"
  transport <- newMemoryTransport
  finally (do
          endpoint <- newEndpoint transport
          Right () <- bindEndpoint endpoint name1
          return ())
      (shutdown transport)

testEndpointBindUnbind :: Assertion
testEndpointBindUnbind = do
  let name1 = Name "endpoint1"
  transport <- newMemoryTransport
  finally (do
          endpoint <- newEndpoint transport
          Right () <- bindEndpoint endpoint name1
          unbound <- unbindEndpoint endpoint name1
          case unbound of
            Left err -> assertFailure $ "Unbind failed: " ++ err
            Right () -> assertBool "Unbind succeeded" True
          return ())
      (shutdown transport)
-}
testEndpointSendReceive :: Assertion
testEndpointSendReceive = do
  let name1 = Name "endpoint1"
      name2 = Name "endpoint2"
  transport <- newMemoryTransport
  withTransport transport $ \endpoint1 ->
    withTransport transport $ \endpoint2 ->
      withBinding endpoint1 name1 $
        withBinding endpoint2 name2 $ do
          sendMessage endpoint1 name2 $ encode "hello!"
          msg <- receiveMessage endpoint2
          assertEqual "Received message not same as sent" (Right "hello!") (decode msg)
          return ()
{-
testEndpointSendReceive :: Assertion
testEndpointSendReceive = do
  let name1 = Name "endpoint1"
      name2 = Name "endpoint2"
  transport <- newMemoryTransport
  finally (do
          endpoint1 <- newEndpoint transport
          endpoint2 <- newEndpoint transport
          Right () <- bindEndpoint endpoint1 name1
          Right () <- bindEndpoint endpoint2 name2
          _ <- sendMessage endpoint1 name2 $ encode "hello!"
          msg <- receiveMessage endpoint2
          assertEqual "Received message not same as sent" (Right "hello!") (decode msg)
          return ())
      (shutdown transport)
-}
-- Memory tests

testMemoryTransport :: Assertion
testMemoryTransport = do
    transport <- newMemoryTransport
    shutdown transport
