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
import Network.RPC
import Network.Transport
import TestUtils

-- external imports

import Control.Applicative
import Control.Concurrent.Async

import Data.Serialize

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
    testTransportEndpointSendReceive2SerialClients transport name1 name2,
  testCase  (transportLabel ++ "-rpc-one-hear-call") $
    testTransportOneHearCall transport name1 name2,
  testCase  (transportLabel ++ "-rpc-one-call-hear") $
    testTransportOneCallHear transport name1 name2,
  testCase  (transportLabel ++ "-rpc-concurrent-call-hear") $
    testTransportConcurrentCallHear transport name1 name2,
  testCase  (transportLabel ++ "-rpc-one-handler") $
    testTransportOneHandler transport name1 name2,
  testCase  (transportLabel ++ "-rpc-two-handlers") $
    testTransportTwoHandlers transport name1 name2
  ]

timeLimited :: Assertion -> Assertion
timeLimited assn = timeBound (1 * 1000000 :: Int) assn

testTransportEndpointSendReceive :: IO Transport -> Name -> Name -> Assertion
testTransportEndpointSendReceive transportFactory name1 name2 = timeLimited $ do
  transport <- transportFactory
  withEndpoint2 transport $ \endpoint1 endpoint2 -> do
    withBinding2 transport (endpoint1,name1) (endpoint2,name2) $ do
      withConnection transport endpoint1 name2 $ do
        sendMessage endpoint1 name2 $ encode "hello!"
        msg <- receiveMessage endpoint2
        assertEqual "Received message not same as sent" (Right "hello!") (decode msg)
        return ()

testTransportEndpointSend2Receive2 :: IO Transport -> Name -> Name -> Assertion
testTransportEndpointSend2Receive2 transportFactory name1 name2 = timeLimited $ do
  transport <- transportFactory
  withEndpoint2 transport $ \endpoint1 endpoint2 -> do
    withBinding2 transport (endpoint1,name1) (endpoint2,name2) $ do
      withConnection transport endpoint1 name2 $ do
        sendMessage endpoint1 name2 $ encode "hello!"
        msg1 <- receiveMessage endpoint2
        assertEqual "Received message not same as sent" (Right "hello!") (decode msg1)
        sendMessage endpoint1 name2 $ encode "ciao!"
        msg2 <- receiveMessage endpoint2
        assertEqual "Received message not same as sent" (Right "ciao!") (decode msg2)
        return ()

testTransportEndpointSendReceive2SerialServers :: IO Transport -> Name -> Name -> Assertion
testTransportEndpointSendReceive2SerialServers transportFactory name1 name2 = timeLimited $ do
  transport <- transportFactory
  withEndpoint2 transport $ \endpoint1 endpoint2 -> do
    withBinding transport endpoint1 name1 $ do
      withBinding transport endpoint2 name2 $ do
        withConnection transport endpoint1 name2 $ do
          sendMessage endpoint1 name2 $ encode "hello!"
          msg <- receiveMessage endpoint2
          assertEqual "Received message not same as sent" (Right "hello!") (decode msg)
          return ()
      withBinding transport endpoint2 name2 $ do
        withConnection transport endpoint1 name2 $ do
          sendMessage endpoint1 name2 $ encode "hello!"
          msg <- receiveMessage endpoint2
          assertEqual "Received message not same as sent" (Right "hello!") (decode msg)
          return ()

testTransportEndpointSendReceive2SerialClients :: IO Transport -> Name -> Name -> Assertion
testTransportEndpointSendReceive2SerialClients transportFactory name1 name2 = timeLimited $ do
  transport <- transportFactory
  withEndpoint2 transport $ \endpoint1 endpoint2 -> do
    withBinding transport endpoint2 name2 $ do
      withBinding transport endpoint1 name1 $ do
        withConnection transport endpoint1 name2 $ do
          sendMessage endpoint1 name2 $ encode "hello!"
          msg <- receiveMessage endpoint2
          assertEqual "Received message not same as sent" (Right "hello!") (decode msg)
          return ()
      withBinding transport endpoint1 name1 $ do
        withConnection transport endpoint1 name2 $ do
          sendMessage endpoint1 name2 $ encode "hello!"
          msg <- receiveMessage endpoint2
          assertEqual "Received message not same as sent" (Right "hello!") (decode msg)
          return ()

testTransportOneHearCall :: IO Transport -> Name -> Name -> Assertion
testTransportOneHearCall transportFactory name1 name2 = timeLimited $ do
  transport <- transportFactory
  withEndpoint2 transport $ \endpoint1 endpoint2 ->
    withBinding2 transport (endpoint1,name1) (endpoint2,name2) $ do
      withConnection transport endpoint1 name2 $ do
        _ <- async $ do
            (bytes,reply) <- hear endpoint2 name2 "foo"
            let Right msg = decode bytes
            reply $ encode $ msg ++ "!"
        let cs = newCallSite endpoint1 name1
        bytes <- call cs name2 "foo" $ encode "hello"
        let Right result = decode bytes
        assertEqual "Result not expected value" "hello!" result

testTransportOneCallHear :: IO Transport -> Name -> Name -> Assertion
testTransportOneCallHear transportFactory name1 name2 = timeLimited $ do
  transport <- transportFactory
  withEndpoint transport $ \endpoint1 ->
    withEndpoint transport $ \endpoint2 ->
      withBinding transport endpoint1 name1 $
        withBinding transport endpoint2 name2 $ do
          withConnection transport endpoint1 name2 $ do
            let cs = newCallSite endpoint1 name1
            acall <- async $ call cs name2 "foo" $ encode "hello"
            _ <- async $ do
                (bytes,reply) <- hear endpoint2 name2 "foo"
                let Right msg = decode bytes
                reply $ encode $ msg ++ "!"
            bytes <- wait acall
            let Right result = decode bytes
            assertEqual "Result not expected value" "hello!" result

testTransportConcurrentCallHear :: IO Transport -> Name -> Name -> Assertion
testTransportConcurrentCallHear transportFactory name1 name2 = timeLimited $ do
  transport <- transportFactory
  withEndpoint transport $ \endpoint1 ->
    withEndpoint transport $ \endpoint2 ->
      withBinding transport endpoint1 name1 $
        withBinding transport endpoint2 name2 $
          withConnection transport endpoint1 name2 $ do
            let cs1 = newCallSite endpoint1 name1
                cs2 = newCallSite endpoint2 name2
            let call1 = call cs1 name2 "foo" $ encode "hello"
                hear1 = do
                    (bytes,reply) <- hear endpoint2 name2 "foo"
                    let Right msg = decode bytes
                    reply $ encode $ msg ++ "!"
                call2 = call cs2 name1 "bar" $ encode "ciao"
                hear2 = do
                    (bytes,reply) <- hear endpoint1 name1 "bar"
                    let Right msg = decode bytes
                    reply $ encode $ msg ++ "!"
            (result1,(),result2,()) <- runConcurrently $ (,,,)
                <$> Concurrently call1
                <*> Concurrently hear1
                <*> Concurrently call2
                <*> Concurrently hear2
            assertEqual "Result not expected value" (Right "hello!") (decode result1)
            assertEqual "Result not expected value" (Right "ciao!")  (decode result2)

testTransportOneHandler :: IO Transport -> Name -> Name -> Assertion
testTransportOneHandler transportFactory name1 name2 = timeLimited $ do
  transport <- transportFactory
  withEndpoint transport $ \endpoint1 ->
    withEndpoint transport $ \endpoint2 ->
      withBinding transport endpoint1 name1 $
        withBinding transport endpoint2 name2 $
          withConnection transport endpoint1 name2 $ do
            h <- handle endpoint2 name2 "foo" $ \bytes ->
                let Right msg = decode bytes
                in return $ encode $ msg ++ "!"
            let cs = newCallSite endpoint1 name1
            bytes <- call cs name2 "foo" $ encode "hello"
            let Right result = decode bytes
            assertEqual "Result not expected value" "hello!" result
            hangup h

testTransportTwoHandlers :: IO Transport -> Name -> Name -> Assertion
testTransportTwoHandlers transportFactory name1 name2 = timeLimited $ do
  transport <- transportFactory
  withEndpoint transport $ \endpoint1 ->
    withEndpoint transport $ \endpoint2 ->
      withBinding transport endpoint1 name1 $
        withBinding transport endpoint2 name2 $
          withConnection transport endpoint1 name2 $ do
            h1 <- handle endpoint2 name2 "foo" $ \bytes ->
                let Right msg = decode bytes
                in return $ encode $ msg ++ "!"
            h2 <- handle endpoint2 name2 "bar" $ \bytes ->
                let Right msg = decode bytes
                in return $ encode $ msg ++ "?"
            let cs = newCallSite endpoint1 name1
            bytes1 <- call cs name2 "foo" $ encode "hello"
            let Right result1 = decode bytes1
            assertEqual "Result not expected value" "hello!" result1
            bytes2 <- call cs name2 "bar" $ encode "hello"
            let Right result2 = decode bytes2
            assertEqual "Result not expected value" "hello?" result2
            hangup h1
            hangup h2
