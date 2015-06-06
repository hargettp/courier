module TestUDP where

-- local imports

import Network.Endpoints
import Network.Transport.UDP

import TestTransports
import TestUtils

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

tests :: [Test.Framework.Test]
tests = 
  [
    testCase "udp-endpoints+transport" testEndpointTransport,

    testCase "udp-bind-unbind" $ endpointBindUnbind _log newUDPTransport newUDPAddress,
    testCase "udp-send-receive" $ endpointSendReceive _log newUDPTransport newUDPAddress,
    testCase "udp-double-send-receive" $ endpointDoubleSendReceive _log newUDPTransport newUDPAddress,
    testCase "udp-send-receive-reply" $ endpointSendReceiveReply _log newUDPTransport newUDPAddress,
    testCase "udp-multiple-send-receive-reply" $ endpointMultipleSendReceiveReply _log newUDPTransport newUDPAddress,
    testCase "udp-local-send-receive-reply" $ endpointLocalSendReceiveReply _log newUDPTransport newUDPAddress,
    
    testCase "udp6-endpoints+transport" $ whenIPv6 $ testEndpointTransport,

    testCase "udp6-bind-unbind" $ whenIPv6 $ endpointBindUnbind _log newUDPTransport6 newUDPAddress6,
    testCase "udp6-send-receive" $ whenIPv6 $ endpointSendReceive _log newUDPTransport6 newUDPAddress6,
    testCase "udp6-double-send-receive" $ whenIPv6 $ endpointDoubleSendReceive _log newUDPTransport6 newUDPAddress6,
    testCase "udp6-send-receive-reply" $ whenIPv6 $ endpointSendReceiveReply _log newUDPTransport6 newUDPAddress6,
    testCase "udp6-multiple-send-receive-reply" $ whenIPv6 $ endpointMultipleSendReceiveReply _log newUDPTransport6 newUDPAddress6,
    testCase "udp6-local-send-receive-reply" $ whenIPv6 $ endpointLocalSendReceiveReply _log newUDPTransport6 newUDPAddress6
  ]

testEndpointTransport :: Assertion
testEndpointTransport = do
  let name1 = Name "endpoint1"
      name2 = Name "endpoint2"
  address1 <- newUDPAddress
  address2 <- newUDPAddress
  let resolver = resolverFromList [(name1,address1),
                               (name2,address2)]
  bracket (newUDPTransport resolver)
          shutdown
          (\transport -> do 
            _ <- newEndpoint transport
            return ())
