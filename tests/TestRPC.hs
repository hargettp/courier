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

import Control.Concurrent
import Control.Concurrent.Async

import qualified Data.Map as M

import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

_log :: String
_log = "test.rpc"

tests :: [Test.Framework.Test]
tests = [
    testCase "call-one-hear" testOneHear,
    testCase "call-one-handler" testOneHandler,
    testCase "call-two-handlers" testTwoHandlers,
    testCase "gcall-three-handlers" testGroupCall,
    testCase "call-one-with-timeout" testOneHandlerWithTimeout,
    testCase "gcall-three-handlers-with-timeout"testGroupCallWithTimeout
  ]

testOneHear :: Assertion
testOneHear = do
    let name1 = "endpoint1"
        name2 = "endpoint2"
    transport <- newMemoryTransport
    endpoint1 <- newEndpoint [transport]
    endpoint2 <- newEndpoint [transport]
    Right () <- bindEndpoint endpoint1 name1
    Right () <- bindEndpoint endpoint2 name2
    _ <- async $ do
        (msg,reply) <- hear endpoint2 name2 "foo"
        reply $ msg ++ "!"
    let cs = newCallSite endpoint1 name1
    result <- call cs name2 "foo" "hello"
    assertEqual "Result not expected value" "hello!" result

testOneHandler :: Assertion
testOneHandler = do
    let name1 = "endpoint1"
        name2 = "endpoint2"
    transport <- newMemoryTransport
    endpoint1 <- newEndpoint [transport]
    endpoint2 <- newEndpoint [transport]
    Right () <- bindEndpoint endpoint1 name1
    Right () <- bindEndpoint endpoint2 name2
    h <- handle endpoint2 name2 "foo" $ \msg ->
        return $ msg ++ "!"
    let cs = newCallSite endpoint1 name1
    result <- call cs name2 "foo" "hello"
    assertEqual "Result not expected value" "hello!" result
    hangup h

testTwoHandlers :: Assertion
testTwoHandlers = do
    let name1 = "endpoint1"
        name2 = "endpoint2"
    transport <- newMemoryTransport
    endpoint1 <- newEndpoint [transport]
    endpoint2 <- newEndpoint [transport]
    Right () <- bindEndpoint endpoint1 name1
    Right () <- bindEndpoint endpoint2 name2
    h1 <- handle endpoint2 name2 "foo" $ \msg ->
        return $ msg ++ "!"
    h2 <- handle endpoint2 name2 "bar" $ \msg ->
        return $ msg ++ "?"
    let cs = newCallSite endpoint1 name1
    result1 <- call cs name2 "foo" "hello"
    assertEqual "Result not expected value" "hello!" result1
    result2 <- call cs name2 "bar" "hello"
    assertEqual "Result not expected value" "hello?" result2
    hangup h1
    hangup h2

testGroupCall :: Assertion
testGroupCall = do
    let name1 = "endpoint1"
        name2 = "endpoint2"
        name3 = "endpoint3"
        name4 = "endpoint4"
    transport <- newMemoryTransport
    endpoint1 <- newEndpoint [transport]
    endpoint2 <- newEndpoint [transport]
    endpoint3 <- newEndpoint [transport]
    endpoint4 <- newEndpoint [transport]
    Right () <- bindEndpoint endpoint1 name1
    Right () <- bindEndpoint endpoint2 name2
    Right () <- bindEndpoint endpoint3 name3
    Right () <- bindEndpoint endpoint4 name4
    h2 <- handle endpoint2 name2 "foo" $ \msg -> if msg == "hello" then return "foo" else return ""
    h3 <- handle endpoint3 name3 "foo" $ \msg -> if msg == "hello" then return "bar" else return ""
    h4 <- handle endpoint4 name4 "foo" $ \msg -> if msg == "hello" then return "baz" else return ""
    let cs = newCallSite endpoint1 name1
    results <- (gcall cs [name2,name3,name4] "foo" "hello") :: IO (M.Map Name String)
    assertBool "Foo not present in results" (elem "foo" $ M.elems results)
    assertBool "Bar not present in results" (elem "bar" $ M.elems results)
    assertBool "Bar not present in results" (elem "baz" $ M.elems results)
    assertEqual "Unxpected number of results" 3 (M.size results)
    hangup h2
    hangup h3
    hangup h4

testOneHandlerWithTimeout :: Assertion
testOneHandlerWithTimeout = do
    let name1 = "endpoint1"
        name2 = "endpoint2"
        longer = 500 * 1000 -- half a second
        shorter = 250 * 1000 -- quarter second
    transport <- newMemoryTransport
    endpoint1 <- newEndpoint [transport]
    endpoint2 <- newEndpoint [transport]
    Right () <- bindEndpoint endpoint1 name1
    Right () <- bindEndpoint endpoint2 name2
    -- first call with caller waiting longer than handler
    h1 <- handle endpoint2 name2 "foo" $ \msg -> do
        threadDelay shorter
        return $ msg ++ "!"
    let cs1 = newCallSite endpoint1 name1
    result1 <- callWithTimeout cs1 name2 "foo" longer "hello"
    assertEqual "Result not expected value" (Just "hello!") result1
    hangup h1
    -- now call with handler waiting longer than caller
    h2 <- handle endpoint2 name2 "foo" $ \msg -> do
        threadDelay longer
        return $ msg ++ "!"
    let cs2 = newCallSite endpoint1 name1
    result2 <- (callWithTimeout cs2 name2 "foo" shorter "hello") :: IO (Maybe String)
    assertEqual "Result not expected value" Nothing result2
    hangup h2

testGroupCallWithTimeout :: Assertion
testGroupCallWithTimeout = do
    let name1 = "endpoint1"
        name2 = "endpoint2"
        name3 = "endpoint3"
        name4 = "endpoint4"
        longest = 750 * 1000 -- three quarters of a second
        longer = 500 * 1000 -- half a second
        shorter = 250 * 1000 -- quarter second
    transport <- newMemoryTransport
    endpoint1 <- newEndpoint [transport]
    endpoint2 <- newEndpoint [transport]
    endpoint3 <- newEndpoint [transport]
    endpoint4 <- newEndpoint [transport]
    Right () <- bindEndpoint endpoint1 name1
    Right () <- bindEndpoint endpoint2 name2
    Right () <- bindEndpoint endpoint3 name3
    Right () <- bindEndpoint endpoint4 name4
    h2 <- handle endpoint2 name2 "foo" $ \msg -> do
        threadDelay shorter
        if msg == "hello" then return "foo" else return ""
    h3 <- handle endpoint3 name3 "foo" $ \msg ->  do
        threadDelay shorter
        if msg == "hello" then return "bar" else return ""
    h4 <- handle endpoint4 name4 "foo" $ \msg ->  do
        threadDelay longest
        if msg == "hello" then return "baz" else return ""
    let cs = newCallSite endpoint1 name1
    results <- gcallWithTimeout cs [name2,name3,name4] "foo" longer "hello"
    assertEqual "Foo not present in results" (Just $ Just "foo") (M.lookup name2 results)
    assertEqual "Bar not present in results" (Just $ Just "bar") (M.lookup name3 results)
    assertEqual "Baz shouldn't be present in results" (Just $ Nothing) (M.lookup name4 results)
    assertEqual "Unxpected number of results" 3 (M.size results)
    hangup h2
    hangup h3
    hangup h4
