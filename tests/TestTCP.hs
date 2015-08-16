module TestTCP (tests) where

-- local imports

import Network.Endpoints
import Network.Transport.Sockets.TCP

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
    testCase "tcp-endpoints+transport" testEndpointTransport,
    testCase "tcp-sendReceive" testEndpointSendReceive,
    testCase "tcp-transport" testTCPTransport
  ]

testEndpointTransport :: Assertion
testEndpointTransport = do
  transport <- newTCPTransport4 tcpSocketResolver4
  withEndpoint transport $ \_ -> return ()

_log :: String
_log = "_test_TCP"

testEndpointSendReceive =
  testTransportEndpointSendReceive
    (newTCPTransport4 tcpSocketResolver4) (Name "localhost:9001") (Name "localhost:9002")

-- TCP tests

testTCPTransport :: Assertion
testTCPTransport = do
    transport <- newTCPTransport4 tcpSocketResolver4
    shutdown transport
