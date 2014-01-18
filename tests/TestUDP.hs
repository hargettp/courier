module TestUDP where

-- local imports

import Network.Endpoints
import Network.Transport.UDP

import TestCommon

-- external imports

import Control.Exception

import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit
-- import Test.Framework.Providers.QuickCheck2

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

_log :: String
_log = "test.transport.udp"

address1 :: Address
address1 = "localhost:2003"

address2 :: Address
address2 = "localhost:2004"

tests :: [Test.Framework.Test]
tests = 
  [
    testCase "udp-endpoints+transport" testEndpointTransport,

    testCase "udp-bind-unbind" $ endpointBindUnbind _log newUDPTransport address1 address2,
    testCase "udp-send-receive" $ endpointSendReceive _log newUDPTransport address1 address2,
    testCase "udp-double-send-receive" $ endpointDoubleSendReceive _log newUDPTransport address1 address2,
    testCase "udp-send-receive-reply" $ endpointSendReceiveReply _log newUDPTransport address1 address2,
    testCase "udp-multiple-send-receive-reply" $ endpointMultipleSendReceiveReply _log newUDPTransport address1 address2,
    testCase "udp-local-send-receive-reply" $ endpointLocalSendReceiveReply _log newUDPTransport address1 address2
    
  ]

testEndpointTransport :: Assertion
testEndpointTransport = do
  let name1 = "endpoint1"
      name2 = "endpoint2"
  let resolver = resolverFromList [(name1,address1),
                               (name2,address2)]
  bracket (newUDPTransport resolver)
          shutdown
          (\transport -> do 
            _ <- newEndpoint [transport]
            return ())
