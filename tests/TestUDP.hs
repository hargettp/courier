module TestUDP (
  tests4
  ,tests6
) where

-- local imports

import Network.Endpoints
import Network.Transport.Sockets.UDP

import TransportTestSuite

-- external imports

import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

tests4 :: [Test.Framework.Test]
tests4 = [
    testCase "udp-endpoints+transport" testEndpointTransport,
    testCase "udp-transport" testUDPTransport
  ]
  ++ transportTestSuite
    (newUDPTransport4 udpSocketResolver4)
    "udp4"
    (Name "localhost:9001")
    (Name "localhost:9002")
    (Name "localhost:9003")
    (Name "localhost:9004")

tests6 :: [Test.Framework.Test]
tests6 = transportTestSuite
  (newUDPTransport6 udpSocketResolver6)
  "udp6"
  (Name "localhost:9001")
  (Name "localhost:9002")
  (Name "localhost:9003")
  (Name "localhost:9004")

testEndpointTransport :: Assertion
testEndpointTransport = do
  transport <- newUDPTransport4 udpSocketResolver4
  withEndpoint transport $ \_ -> return ()

_log :: String
_log = "_test_UDP"

-- UDP tests

testUDPTransport :: Assertion
testUDPTransport = do
    transport <- newUDPTransport4 udpSocketResolver4
    shutdown transport
