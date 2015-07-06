-----------------------------------------------------------------------------
-- |
-- Module      :  TestRPC
-- Copyright   :  (c) Phil Hargett 2014
-- License     :  MIT (see LICENSE file)
--
-- Maintainer  :  phil@haphazardhouse.net
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- (..... module description .....)
--
-----------------------------------------------------------------------------

module TestRPC (
    tests
) where

-- local imports

import Network.Endpoints
import Network.RPC
import Network.Transport.Memory

-- external imports

import Control.Applicative
import Control.Concurrent.Async

import qualified Data.Map as M
import Data.Serialize

import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

_log :: String
_log = "test.rpc"

tests :: [Test.Framework.Test]
tests = [
    testCase "call-one-hear-call" testOneHearCall,
    testCase "call-one-call-hear" testOneCallHear,
    testCase "call-concurrent-call-hear" testConcurrentCallHear,
    testCase "call-one-handler" testOneHandler,
    testCase "call-two-handlers" testTwoHandlers,
    testCase "gcall-three-handlers" testGroupCall
    {-
    ,
    testCase "anycall-three-handlers" testAnyCall,
    testCase "call-one-with-timeout" testOneHandlerWithTimeout,
    testCase "gcall-three-handlers-with-timeout"testGroupCallWithTimeout
    -}
  ]

testOneHearCall :: Assertion
testOneHearCall = do
    let name1 = Name "endpoint1"
        name2 = Name "endpoint2"
    transport <- newMemoryTransport
    withEndpoint2 transport $ \endpoint1 endpoint2 ->
      withBinding2 (endpoint1,name1) (endpoint2,name2) $ do
        withConnection endpoint1 name2 $ do
          _ <- async $ do
              (bytes,reply) <- hear endpoint2 name2 "foo"
              let Right msg = decode bytes
              reply $ encode $ msg ++ "!"
          let cs = newCallSite endpoint1 name1
          bytes <- call cs name2 "foo" $ encode "hello"
          let Right result = decode bytes
          assertEqual "Result not expected value" "hello!" result

testOneCallHear :: Assertion
testOneCallHear = do
  let name1 = Name "endpoint1"
      name2 = Name "endpoint2"
  transport <- newMemoryTransport
  withEndpoint transport $ \endpoint1 ->
    withEndpoint transport $ \endpoint2 ->
      withBinding endpoint1 name1 $
        withBinding endpoint2 name2 $ do
          withConnection endpoint1 name2 $ do
            let cs = newCallSite endpoint1 name1
            acall <- async $ call cs name2 "foo" $ encode "hello"
            _ <- async $ do
                (bytes,reply) <- hear endpoint2 name2 "foo"
                let Right msg = decode bytes
                reply $ encode $ msg ++ "!"
            bytes <- wait acall
            let Right result = decode bytes
            assertEqual "Result not expected value" "hello!" result

testConcurrentCallHear :: Assertion
testConcurrentCallHear = do
  let name1 = Name "endpoint1"
      name2 = Name "endpoint2"
  transport <- newMemoryTransport
  withEndpoint transport $ \endpoint1 ->
    withEndpoint transport $ \endpoint2 ->
      withBinding endpoint1 name1 $
        withBinding endpoint2 name2 $
          withConnection endpoint1 name2 $ do
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

testOneHandler :: Assertion
testOneHandler = do
  let name1 = Name "endpoint1"
      name2 = Name "endpoint2"
  transport <- newMemoryTransport
  withEndpoint transport $ \endpoint1 ->
    withEndpoint transport $ \endpoint2 ->
      withBinding endpoint1 name1 $
        withBinding endpoint2 name2 $
          withConnection endpoint1 name2 $ do
            h <- handle endpoint2 name2 "foo" $ \bytes ->
                let Right msg = decode bytes
                in return $ encode $ msg ++ "!"
            let cs = newCallSite endpoint1 name1
            bytes <- call cs name2 "foo" $ encode "hello"
            let Right result = decode bytes
            assertEqual "Result not expected value" "hello!" result
            hangup h

testTwoHandlers :: Assertion
testTwoHandlers = do
  let name1 = Name "endpoint1"
      name2 = Name "endpoint2"
  transport <- newMemoryTransport
  withEndpoint transport $ \endpoint1 ->
    withEndpoint transport $ \endpoint2 ->
      withBinding endpoint1 name1 $
        withBinding endpoint2 name2 $
          withConnection endpoint1 name2 $ do
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

testGroupCall :: Assertion
testGroupCall = do
    let name1 = Name "endpoint1"
        name2 = Name "endpoint2"
        name3 = Name "endpoint3"
        name4 = Name "endpoint4"
    transport <- newMemoryTransport
    withEndpoint4 transport $ \endpoint1 endpoint2 endpoint3 endpoint4 -> do
      withBinding4 (endpoint1,name1) (endpoint2,name2) (endpoint3,name3) (endpoint4,name4) $
        withConnection3 endpoint1 name2 name3 name4 $ do
          h2 <- handle endpoint2 name2 "foo" $ \bytes -> let Right msg = decode bytes in
                                                            return $ encode $ if msg == "hello" then "foo" else ""
          h3 <- handle endpoint3 name3 "foo" $ \bytes -> let Right msg = decode bytes in
                                                             return $ encode $ if msg == "hello" then "bar" else ""
          h4 <- handle endpoint4 name4 "foo" $ \bytes -> let Right msg = decode bytes in
                                                             return $ encode $ if msg == "hello" then "baz" else ""
          let cs = newCallSite endpoint1 name1
          results <- (gcall cs [name2,name3,name4] "foo" $ encode "hello")
          assertBool "Foo not present in results" (elem (encode "foo") $ M.elems results)
          assertBool "Bar not present in results" (elem (encode "bar") $ M.elems results)
          assertBool "Bar not present in results" (elem (encode "baz") $ M.elems results)
          assertEqual "Unxpected number of results" 3 (M.size results)
          hangup h2
          hangup h3
          hangup h4

{-
testAnyCall :: Assertion
testAnyCall = do
    let name1 = Name "endpoint1"
        name2 = Name "endpoint2"
        name3 = Name "endpoint3"
        name4 = Name "endpoint4"
    transport <- newMemoryTransport
    endpoint1 <- newEndpoint transport
    endpoint2 <- newEndpoint transport
    endpoint3 <- newEndpoint transport
    endpoint4 <- newEndpoint transport
    Right () <- bindEndpoint endpoint1 name1
    Right () <- bindEndpoint endpoint2 name2
    Right () <- bindEndpoint endpoint3 name3
    Right () <- bindEndpoint endpoint4 name4
    h2 <- handle endpoint2 name2 "foo" $ \bytes -> let Right msg = decode bytes in
                                                      return $ encode $ if msg == "hello" then "foo" else ""
    h3 <- handle endpoint3 name3 "foo" $ \bytes -> let Right msg = decode bytes in
                                                       return $ encode $ if msg == "hello" then "foo" else ""
    h4 <- handle endpoint4 name4 "foo" $ \bytes -> let Right msg = decode bytes in
                                                       return $ encode $ if msg == "hello" then "foo" else ""
    let cs = newCallSite endpoint1 name1
    (result,responder) <- (anyCall cs [name2,name3,name4] "foo" $ encode "hello")
    assertEqual "Response should have been 'foo'" (encode "foo") result
    assertBool "Responder was not in original list of names" $ elem responder [name2,name3,name4]
    hangup h2
    hangup h3
    hangup h4

testOneHandlerWithTimeout :: Assertion
testOneHandlerWithTimeout = do
    let name1 = Name "endpoint1"
        name2 = Name "endpoint2"
        longer = 500 * 1000 -- half a second
        shorter = 250 * 1000 -- quarter second
    transport <- newMemoryTransport
    endpoint1 <- newEndpoint transport
    endpoint2 <- newEndpoint transport
    Right () <- bindEndpoint endpoint1 name1
    Right () <- bindEndpoint endpoint2 name2
    -- first call with caller waiting longer than handler
    h1 <- handle endpoint2 name2 "foo" $ \bytes -> do
        let Right msg = decode bytes
        threadDelay shorter
        return $ encode $ msg ++ "!"
    let cs1 = newCallSite endpoint1 name1
    Just bytes1 <- callWithTimeout cs1 name2 "foo" longer $ encode "hello"
    let Right result1 = decode bytes1
    assertEqual "Result not expected value" (Just "hello!") (Just result1)
    hangup h1
    -- now call with handler waiting longer than caller
    h2 <- handle endpoint2 name2 "foo" $ \bytes -> do
        let Right msg = decode bytes
        threadDelay longer
        return $ encode $ msg ++ "!"
    let cs2 = newCallSite endpoint1 name1
    bytes2 <- (callWithTimeout cs2 name2 "foo" shorter $ encode "hello")
    assertEqual "Result not expected value" Nothing bytes2
    hangup h2

testGroupCallWithTimeout :: Assertion
testGroupCallWithTimeout = do
    let name1 = Name "endpoint1"
        name2 = Name "endpoint2"
        name3 = Name "endpoint3"
        name4 = Name "endpoint4"
        longest = 750 * 1000 -- three quarters of a second
        longer = 500 * 1000 -- half a second
        shorter = 250 * 1000 -- quarter second
    transport <- newMemoryTransport
    endpoint1 <- newEndpoint transport
    endpoint2 <- newEndpoint transport
    endpoint3 <- newEndpoint transport
    endpoint4 <- newEndpoint transport
    Right () <- bindEndpoint endpoint1 name1
    Right () <- bindEndpoint endpoint2 name2
    Right () <- bindEndpoint endpoint3 name3
    Right () <- bindEndpoint endpoint4 name4
    h2 <- handle endpoint2 name2 "foo" $ \bytes -> do
        let Right msg = decode bytes
        threadDelay shorter
        return $ encode $ if msg == "hello" then "foo" else ""
    h3 <- handle endpoint3 name3 "foo" $ \bytes ->  do
        threadDelay shorter
        let Right msg = decode bytes
        return $ encode $ if msg == "hello" then "bar" else ""
    h4 <- handle endpoint4 name4 "foo" $ \bytes ->  do
        threadDelay longest
        let Right msg = decode bytes
        return $ encode $ if msg == "hello" then "baz" else ""
    let cs = newCallSite endpoint1 name1
    results <- gcallWithTimeout cs [name2,name3,name4] "foo" longer $ encode "hello"
    assertEqual "Foo not present in results" (Just $ Just $ encode "foo") (M.lookup name2 results)
    assertEqual "Bar not present in results" (Just $ Just $ encode "bar") (M.lookup name3 results)
    assertEqual "Baz shouldn't be present in results" (Just $ Nothing) (M.lookup name4 results)
    assertEqual "Unxpected number of results" 3 (M.size results)
    hangup h2
    hangup h3
    hangup h4
-}
