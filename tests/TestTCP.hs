module TestTCP (tests) where

-- local imports

import Network.Endpoints
import Network.Transport.TCP

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
_log = "test.transport.tcp"

maxTestDelay :: Int
maxTestDelay = testDelay * 3

tests :: [Test.Framework.Test]
tests =
  [
    testCase "tcp-endpoints+transport" $ timeBound maxTestDelay $ testEndpointTransport newTCPTransport,
    -- testCase "tcp-bind-unbind" $ timeBound maxTestDelay $ endpointBindUnbind _log newTCPTransport newTCPAddress,
    testCase "tcp-send-receive" $ timeBound maxTestDelay $ endpointSendReceive _log newTCPTransport newTCPAddress,
    testCase "tcp-double-send-receive" $ timeBound maxTestDelay $ endpointDoubleSendReceive _log newTCPTransport newTCPAddress,
    testCase "tcp-send-receive-reply" $ timeBound maxTestDelay $ endpointSendReceiveReply _log newTCPTransport newTCPAddress,
    {-
    testCase "tcp-multiple-client-send-receive-reply" $ timeBound (10 * testDelay) $ 
        troubleshoot $ endpointMultipleServerSendReceiveReply _log newTCPTransport newTCPAddress --,
    -}
    testCase "tcp-multiple-send-receive-reply" $ timeBound maxTestDelay $ endpointMultipleSendReceiveReply _log newTCPTransport newTCPAddress,
    testCase "tcp-local-send-receive-reply" $ timeBound maxTestDelay $ endpointLocalSendReceiveReply _log newTCPTransport newTCPAddress,
    
    testCase "tcp6-endpoints+transport" $ whenIPv6 $ testEndpointTransport newTCPTransport6,
    testCase "tcp6-bind-unbind" $ whenIPv6 $ endpointBindUnbind _log newTCPTransport6 newTCPAddress6,
    testCase "tcp6-send-receive" $ whenIPv6 $ endpointSendReceive _log newTCPTransport6 newTCPAddress6,
    testCase "tcp6-double-send-receive" $ whenIPv6 $ endpointDoubleSendReceive _log newTCPTransport6 newTCPAddress6,
    testCase "tcp6-send-receive-reply" $ whenIPv6 $ endpointSendReceiveReply _log newTCPTransport6 newTCPAddress6,
    -- testCase "tcp6-multiple-client-send-receive-reply" $ endpointMultipleClientSendReceiveReply _log newTCPTransport6 newTCPAddress6,
    testCase "tcp6-multiple-send-receive-reply" $ whenIPv6 $ endpointMultipleSendReceiveReply _log newTCPTransport6 newTCPAddress6,
    testCase "tcp6-local-send-receive-reply" $ whenIPv6 $ endpointLocalSendReceiveReply _log newTCPTransport6 newTCPAddress6
  ]

testEndpointTransport :: (Resolver -> IO Transport) -> Assertion
testEndpointTransport transportFactory = do
  let name1 = Name "endpoint1"
      name2 = Name "endpoint2"
  address1 <- newTCPAddress
  address2 <- newTCPAddress
  let resolver = resolverFromList [(name1,address1),
                               (name2,address2)]
  bracket (transportFactory resolver)
          shutdown
          (\transport -> do
            _ <- newEndpoint transport
            return ())
