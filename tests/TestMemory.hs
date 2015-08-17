module TestMemory (tests) where

-- local imports

import Network.Endpoints
import Network.Transport.Memory

import TransportTestSuite

-- external imports

import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

tests :: [Test.Framework.Test]
tests = [
    testCase "mem-endpoints+transport" testEndpointTransport,
    testCase "mem-transport" testMemoryTransport
  ]
  ++ transportTestSuite
    newMemoryTransport
    "mem"
    (Name "name1")
    (Name "name2")

testEndpointTransport :: Assertion
testEndpointTransport = do
  transport <- newMemoryTransport
  withEndpoint transport $ \_ -> return ()

_log :: String
_log = "_test_memory"


-- Memory tests

testMemoryTransport :: Assertion
testMemoryTransport = do
    transport <- newMemoryTransport
    shutdown transport
