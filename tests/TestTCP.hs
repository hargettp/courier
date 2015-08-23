module TestTCP (
  tests4
  ,tests6
  ) where

-- local imports

import Network.Transport.Sockets.TCP

import TransportTestSuite

-- external imports

import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

tests4 :: IO [Test.Framework.Test]
tests4 = do
  name1 <- newTCPAddress
  name2 <- newTCPAddress
  name3 <- newTCPAddress
  name4 <- newTCPAddress
  return $ [
      testCase "tcp-endpoints+transport" testEndpointTransport,
      testCase "tcp-transport" testTCPTransport
    ]
    ++ transportTestSuite
      (newTCPTransport4 tcpSocketResolver4)
      "tcp4"
      name1
      name2
      name3
      name4

tests6 :: IO [Test.Framework.Test]
tests6 = do
  name1 <- newTCPAddress6
  name2 <- newTCPAddress6
  name3 <- newTCPAddress6
  name4 <- newTCPAddress6
  return $ transportTestSuite
    (newTCPTransport6 tcpSocketResolver6)
    "tcp6"
    name1
    name2
    name3
    name4

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
