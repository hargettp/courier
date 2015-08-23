module TestUDP (
  tests4
  ,tests6
) where

-- local imports

import Network.Transport.Sockets.UDP

import TransportTestSuite

-- external imports

import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

tests4 :: IO [Test.Framework.Test]
tests4 = do
  name1 <- newUDPAddress
  name2 <- newUDPAddress
  name3 <- newUDPAddress
  name4 <- newUDPAddress
  return $ [
      testCase "udp-endpoints+transport" testEndpointTransport,
      testCase "udp-transport" testUDPTransport
    ]
    ++ transportTestSuite
      (newUDPTransport4 udpSocketResolver4)
      "udp4"
      name1
      name2
      name3
      name4

tests6 :: IO [Test.Framework.Test]
tests6 = do
  name1 <- newUDPAddress6
  name2 <- newUDPAddress6
  name3 <- newUDPAddress6
  name4 <- newUDPAddress6
  return $ transportTestSuite
    (newUDPTransport6 udpSocketResolver6)
    "udp6"
    name1
    name2
    name3
    name4

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
