module TestTCP (tests) where

-- local imports

import Network.Endpoints
import Network.Transport.TCP

-- external imports

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
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
  let resolver = resolverFromList [(name1,newTCPAddress "localhost:2000"),
                               (name2,newTCPAddress "localhost:2001")]
  transport <- newTCPTransport resolver
  _ <- newEndpoint [transport]
  return ()
  
testEndpointBindUnbind :: Assertion
testEndpointBindUnbind = do  
  let name1 = "endpoint1"
      name2 = "endpoint2"
  let resolver = resolverFromList [(name1,newTCPAddress "localhost:2000"),
                               (name2,newTCPAddress "localhost:2001")]
  transport <- newTCPTransport resolver
  endpoint <- newEndpoint [transport]
  Right () <- bindEndpoint endpoint name1
  unbound <- unbindEndpoint endpoint name1
  case unbound of
    Left err -> assertFailure $ "Unbind failed: " ++ err
    Right () -> assertBool "Unbind succeeded" True
  return ()
  
testEndpointSendReceive :: Assertion
testEndpointSendReceive = do
  let name1 = "endpoint1"
      name2 = "endpoint2"
  let resolver = resolverFromList [(name1,newTCPAddress "localhost:2000"),
                               (name2,newTCPAddress "localhost:2001")]
  bracket (newTCPTransport resolver)
    (\transport1 -> shutdown transport1)
    (\transport1 -> 
        bracket (newTCPTransport resolver)
          (\transport2 -> shutdown transport2)
          (\transport2 -> do 
              endpoint1 <- newEndpoint [transport1]
              endpoint2 <- newEndpoint [transport2]
              Right () <- bindEndpoint endpoint1 name1
              Right () <- bindEndpoint endpoint2 name2
              threadDelay $ 1 * 1000000
              _ <- sendMessage endpoint1 name2 $ encode "hello!"
              msgVar <- atomically $ newTVar Nothing
              withAsync (do
                            msg <- receiveMessage endpoint2    
                            atomically $ writeTVar msgVar $ Just msg)
                (\_ -> do 
                    threadDelay (2 * 1000000)
                    Just msg <- atomically $ readTVar msgVar
                    assertEqual "Received message not same as sent" (Right "hello!") (decode msg))
              return ()))
  
testEndpointSendReceiveReply :: Assertion
testEndpointSendReceiveReply = do
  let name1 = "endpoint1"
      name2 = "endpoint2"
  let resolver = resolverFromList [(name1,newTCPAddress "localhost:2002"),
                                   (name2,newTCPAddress "localhost:2003")]
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
              threadDelay $ 1 * 1000000
              
              infoM _log "Sending message from 1 to 2"
              _ <- sendMessage endpoint1 name2 $ encode "hello!"
              msgVar1 <- atomically $ newTVar Nothing
              withAsync (do
                            msg <- receiveMessage endpoint2    
                            atomically $ writeTVar msgVar1 $ Just msg)
                (\_ -> do 
                    threadDelay (1 * 1000000)
                    Just msg <- atomically $ readTVar msgVar1
                    assertEqual "Received message not same as sent" (Right "hello!") (decode msg))
                
              infoM _log "Sending message from 2 to 1"
              _ <- sendMessage endpoint2 name1 $ encode "hi!"
              msgVar2 <- atomically $ newTVar Nothing
              withAsync (do
                            msg <- receiveMessage endpoint1
                            atomically $ writeTVar msgVar2 $ Just msg)
                (\_ -> do 
                    threadDelay (1 * 1000000)
                    Just msg <- atomically $ readTVar msgVar2
                    assertEqual "Received message not same as sent" (Right "hi!") (decode msg))
                
              return ()))
  
