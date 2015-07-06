module TestMemory (tests) where

-- local imports

import Network.Endpoints
import Network.Transport.Memory

-- external imports

import Data.Serialize

import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit

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
  withEndpoint transport $ \_ -> return ()

_log :: String
_log = "_test_memory"

testEndpointSendReceive :: Assertion
testEndpointSendReceive = do
  let name1 = Name "endpoint1"
      name2 = Name "endpoint2"
  transport <- newMemoryTransport
  withEndpoint2 transport $ \endpoint1 endpoint2 -> do
    withBinding2 (endpoint1,name1) (endpoint2,name2) $ do
      withConnection endpoint1 name2 $ do
        sendMessage endpoint1 name2 $ encode "hello!"
        msg <- receiveMessage endpoint2
        assertEqual "Received message not same as sent" (Right "hello!") (decode msg)
        return ()

-- Memory tests

testMemoryTransport :: Assertion
testMemoryTransport = do
    transport <- newMemoryTransport
    shutdown transport
