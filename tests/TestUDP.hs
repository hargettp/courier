module TestUDP where

-- local imports

import Network.Endpoints
import Network.Transport.UDP

-- external imports

import Control.Concurrent
import Control.Exception

import Data.Serialize

import System.Log.Logger

import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit
-- import Test.Framework.Providers.QuickCheck2

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

_log :: String
_log = "test.transport.udp"

testDelay :: Int
testDelay = 1 * 1000000

address1 :: Address
address1 = "localhost:2003"

address2 :: Address
address2 = "localhost:2004"

tests :: [Test.Framework.Test]
tests = 
  [
    testCase "udp-endpoints+transport" testEndpointTransport --,
    -- testCase "udp-bind-unbind" testEndpointBindUnbind,
    -- testCase "udp-send-receive" testEndpointSendReceive --,
    {-
    testCase "udp-double-send-receive" testEndpointDoubleSendReceive,
    testCase "udp-send-receive-reply" testEndpointSendReceiveReply,
    testCase "udp-multiple-send-receive-reply" testEndpointMultipleSendReceiveReply,
    testCase "udp-local-send-receive-reply" testEndpointLocalSendReceiveReply
    -}
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

testEndpointBindUnbind :: Assertion
testEndpointBindUnbind = do  
  let name1 = "endpoint1"
      name2 = "endpoint2"
  let resolver = resolverFromList [(name1,address1),
                               (name2,address2)]
  bracket (newUDPTransport resolver)
          shutdown
          (\transport -> do 
            endpoint <- newEndpoint [transport]
            Right () <- bindEndpoint endpoint name1
            unbound <- unbindEndpoint endpoint name1
            case unbound of
              Left err -> assertFailure $ "Unbind failed: " ++ err
              Right () -> assertBool "Unbind succeeded" True
            return ())

testEndpointSendReceive :: Assertion
testEndpointSendReceive = do
  let name1 = "endpoint1"
      name2 = "endpoint2"
  let resolver = resolverFromList [(name1,address1),
                               (name2,address2)]
  bracket (newUDPTransport resolver)
    shutdown
    (\transport1 -> 
        bracket (newUDPTransport resolver)
          shutdown
          (\transport2 -> do 
              endpoint1 <- newEndpoint [transport1]
              endpoint2 <- newEndpoint [transport2]
              Right () <- bindEndpoint endpoint1 name1
              Right () <- bindEndpoint endpoint2 name2
              threadDelay testDelay

              infoM _log "Sending message from 1 to 2"
              sendMessage_ endpoint1 name2 $ encode "hello!"
              Just msg <- receiveMessageTimeout endpoint2 testDelay
              assertEqual "Received message not same as sent" (Right "hello!") (decode msg)
              Right () <- unbindEndpoint endpoint1 name1
              Right () <- unbindEndpoint endpoint2 name2
              return ()))
