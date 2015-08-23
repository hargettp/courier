module TestTCP (
  tests4
  ,tests6
  ) where

-- local imports

import Network.Endpoints
import Network.Transport.Sockets.TCP

import TransportTestSuite

-- external imports

import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

tests4 :: [Test.Framework.Test]
tests4 = [
    testCase "tcp-endpoints+transport" testEndpointTransport,
    testCase "tcp-transport" testTCPTransport
  ]
  ++ transportTestSuite
    (newTCPTransport4 tcpSocketResolver4)
    "tcp4"
    (Name "localhost:9001")
    (Name "localhost:9002")
    (Name "localhost:9003")
    (Name "localhost:9004")

tests6 :: [Test.Framework.Test]
tests6 = transportTestSuite
  (newTCPTransport6 tcpSocketResolver6)
  "tcp6"
  (Name "localhost:9001")
  (Name "localhost:9002")
  (Name "localhost:9003")
  (Name "localhost:9004")

testEndpointTransport :: Assertion
testEndpointTransport = do
  transport <- newTCPTransport4 tcpSocketResolver4
  withEndpoint transport $ \_ -> return ()

_log :: String
_log = "_test_TCP"

-- TCP tests

testTCPTransport :: Assertion
testTCPTransport = do
    transport <- newTCPTransport4 tcpSocketResolver4
    shutdown transport
