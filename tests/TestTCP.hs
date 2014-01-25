module TestTCP (tests) where

-- local imports

import Network.Endpoints
import Network.Transport.TCP

import TestSockets

-- external imports

import Control.Exception


import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit
-- import Test.Framework.Providers.QuickCheck2

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

_log :: String
_log = "test.transport.tcp"

address1 :: Address
address1 = "localhost:2001"

address2 :: Address
address2 = "localhost:2002"

tests :: [Test.Framework.Test]
tests =
  [
    testCase "tcp-endpoints+transport" testEndpointTransport,
    testCase "tcp-bind-unbind" $ endpointBindUnbind _log newTCPTransport address1 address2,
    testCase "tcp-send-receive" $ endpointSendReceive _log newTCPTransport address1 address2,
    testCase "tcp-double-send-receive" $ endpointDoubleSendReceive _log newTCPTransport address1 address2,
    testCase "tcp-send-receive-reply" $ endpointSendReceiveReply _log newTCPTransport address1 address2,
    testCase "tcp-multiple-send-receive-reply" $ endpointMultipleSendReceiveReply _log newTCPTransport address1 address2,
    testCase "tcp-local-send-receive-reply" $ endpointLocalSendReceiveReply _log newTCPTransport address1 address2
  ]

testEndpointTransport :: Assertion
testEndpointTransport = do
  let name1 = "endpoint1"
      name2 = "endpoint2"
  let resolver = resolverFromList [(name1,address1),
                               (name2,address2)]
  bracket (newTCPTransport resolver)
          shutdown
          (\transport -> do
            _ <- newEndpoint [transport]
            return ())
