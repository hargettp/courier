module TestMemory (tests) where

-- local imports

import Network.Endpoints
import Network.Transport.Memory

import TestUtils

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

testEndpointSendReceive = testTransportEndpointSendReceive newMemoryTransport (Name "name1") (Name "name2")

-- Memory tests

testMemoryTransport :: Assertion
testMemoryTransport = do
    transport <- newMemoryTransport
    shutdown transport
