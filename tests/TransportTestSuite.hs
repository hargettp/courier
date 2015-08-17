module TransportTestSuite
    (
    transportTestSuite

    ,testTransportEndpointSendReceive
    ,testTransportEndpointSendReceive2SerialClients
    ,testTransportEndpointSendReceive2SerialServers

    ,module TestUtils
    )
    where

-- local imports

import Network.Endpoints
import TestUtils

-- external imports
-- import Control.Concurrent
-- import Control.Concurrent.Async
-- import Control.Exception

import Data.Serialize

-- import qualified Network.Socket as NS

-- import System.Log.Logger

import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

transportTestSuite :: IO Transport -> String -> Name -> Name -> [Test.Framework.Test]
transportTestSuite transport transportLabel name1 name2 = [
  testCase (transportLabel ++ "-sendReceive") $
    testTransportEndpointSendReceive transport name1 name2,
  testCase (transportLabel ++ "-send2Receive2") $
    testTransportEndpointSend2Receive2 transport name1 name2,
  testCase (transportLabel ++ "-sendReceive-2-serial-servers") $
    testTransportEndpointSendReceive2SerialServers transport name1 name2,
  testCase  (transportLabel ++ "-sendReceive-2-serial-clients") $
    testTransportEndpointSendReceive2SerialClients transport name1 name2
  ]

testTransportEndpointSendReceive :: IO Transport -> Name -> Name -> Assertion
testTransportEndpointSendReceive transportFactory name1 name2 = timeBound (1 * 1000000 :: Int) $ do
  transport <- transportFactory
  withEndpoint2 transport $ \endpoint1 endpoint2 -> do
    withBinding2 (endpoint1,name1) (endpoint2,name2) $ do
      withConnection endpoint1 name2 $ do
        sendMessage endpoint1 name2 $ encode "hello!"
        msg <- receiveMessage endpoint2
        assertEqual "Received message not same as sent" (Right "hello!") (decode msg)
        return ()

testTransportEndpointSend2Receive2 :: IO Transport -> Name -> Name -> Assertion
testTransportEndpointSend2Receive2 transportFactory name1 name2 = timeBound (1 * 1000000 :: Int) $ do
  transport <- transportFactory
  withEndpoint2 transport $ \endpoint1 endpoint2 -> do
    withBinding2 (endpoint1,name1) (endpoint2,name2) $ do
      withConnection endpoint1 name2 $ do
        sendMessage endpoint1 name2 $ encode "hello!"
        msg1 <- receiveMessage endpoint2
        assertEqual "Received message not same as sent" (Right "hello!") (decode msg1)
        sendMessage endpoint1 name2 $ encode "ciao!"
        msg2 <- receiveMessage endpoint2
        assertEqual "Received message not same as sent" (Right "ciao!") (decode msg2)
        return ()

testTransportEndpointSendReceive2SerialServers :: IO Transport -> Name -> Name -> Assertion
testTransportEndpointSendReceive2SerialServers transportFactory name1 name2 = timeBound (1 * 1000000 :: Int) $ do
  transport <- transportFactory
  withEndpoint2 transport $ \endpoint1 endpoint2 -> do
    withBinding endpoint1 name1 $ do
      withBinding endpoint2 name2 $ do
        withConnection endpoint1 name2 $ do
          sendMessage endpoint1 name2 $ encode "hello!"
          msg <- receiveMessage endpoint2
          assertEqual "Received message not same as sent" (Right "hello!") (decode msg)
          return ()
      withBinding endpoint2 name2 $ do
        withConnection endpoint1 name2 $ do
          sendMessage endpoint1 name2 $ encode "hello!"
          msg <- receiveMessage endpoint2
          assertEqual "Received message not same as sent" (Right "hello!") (decode msg)
          return ()

testTransportEndpointSendReceive2SerialClients :: IO Transport -> Name -> Name -> Assertion
testTransportEndpointSendReceive2SerialClients transportFactory name1 name2 = timeBound (1 * 1000000 :: Int) $ do
  transport <- transportFactory
  withEndpoint2 transport $ \endpoint1 endpoint2 -> do
    withBinding endpoint2 name2 $ do
      withBinding endpoint1 name1 $ do
        withConnection endpoint1 name2 $ do
          sendMessage endpoint1 name2 $ encode "hello!"
          msg <- receiveMessage endpoint2
          assertEqual "Received message not same as sent" (Right "hello!") (decode msg)
          return ()
      withBinding endpoint1 name1 $ do
        withConnection endpoint1 name2 $ do
          sendMessage endpoint1 name2 $ encode "hello!"
          msg <- receiveMessage endpoint2
          assertEqual "Received message not same as sent" (Right "hello!") (decode msg)
          return ()
