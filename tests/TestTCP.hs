module TestTCP (tests) where

-- local imports

import Network.Endpoints
import Network.Transport.TCP

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
_log = "test.transport.tcp"

testDelay :: Int
testDelay = 1 * 1000000

address1 :: Address
address1 = "localhost:2001"

address2 :: Address
address2 = "localhost:2002"

tests :: [Test.Framework.Test]
tests = 
  [
    testCase "tcp-endpoints+transport" testEndpointTransport,
    testCase "tcp-bind-unbind" testEndpointBindUnbind,
    testCase "tcp-send-receive" testEndpointSendReceive,
    testCase "tcp-send-receive-reply" testEndpointSendReceiveReply
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
  
testEndpointBindUnbind :: Assertion
testEndpointBindUnbind = do  
  let name1 = "endpoint1"
      name2 = "endpoint2"
  let resolver = resolverFromList [(name1,address1),
                               (name2,address2)]
  bracket (newTCPTransport resolver)
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
  bracket (newTCPTransport resolver)
    shutdown
    (\transport1 -> 
        bracket (newTCPTransport resolver)
          shutdown
          (\transport2 -> do 
              endpoint1 <- newEndpoint [transport1]
              endpoint2 <- newEndpoint [transport2]
              Right () <- bindEndpoint endpoint1 name1
              Right () <- bindEndpoint endpoint2 name2
              threadDelay testDelay

              infoM _log "Sending message from 1 to 2"
              _ <- sendMessage endpoint1 name2 $ encode "hello!"
              Just msg <- receiveMessageTimeout endpoint2 testDelay
              assertEqual "Received message not same as sent" (Right "hello!") (decode msg)
              Right () <- unbindEndpoint endpoint1 name1
              Right () <- unbindEndpoint endpoint2 name2
              return ()))
  

testEndpointSendReceiveReply :: Assertion
testEndpointSendReceiveReply = do
  let name1 = "endpoint1"
      name2 = "endpoint2"
  let resolver = resolverFromList [(name1,address1),
                                   (name2,address2)]
  bracket (newTCPTransport resolver)
    shutdown
    (\transport1 -> 
        bracket (newTCPTransport resolver)
          shutdown
          (\transport2 -> do 
              endpoint1 <- newEndpoint [transport1]
              endpoint2 <- newEndpoint [transport2]
              Right () <- bindEndpoint endpoint1 name1
              Right () <- bindEndpoint endpoint2 name2
              threadDelay testDelay
              
              infoM _log "Sending message from 1 to 2"
              _ <- sendMessage endpoint1 name2 $ encode "hello!"
              Just msg1 <- receiveMessageTimeout endpoint2 testDelay
              assertEqual "Received message not same as sent" (Right "hello!") (decode msg1)
                
              infoM _log "Sending message from 2 to 1"
              _ <- sendMessage endpoint2 name1 $ encode "hi!"
              Just msg2 <- receiveMessageTimeout endpoint1 testDelay
              assertEqual "Received message not same as sent" (Right "hi!") (decode msg2)
              Right () <- unbindEndpoint endpoint1 name1
              Right () <- unbindEndpoint endpoint2 name2
                
              return ()))
  
