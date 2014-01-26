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

import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

_log :: String
_log = "test.rpc"

tests :: [Test.Framework.Test]
tests = [
    testCase "call-foo" testOneRPC,
    testCase "call-foo-bar" testTwoRPCs,
    testCase "gcall-foo-bar-baz" testGroupCall
  ]

testOneRPC :: Assertion
testOneRPC = do
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

testTwoRPCs :: Assertion
testTwoRPCs = do
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
    results <- gcall cs [name2,name3,name4] "foo" "hello"
    assertBool "Foo not present in results" (elem "foo" results)
    assertBool "Bar not present in results" (elem "bar" results)
    assertBool "Bar not present in results" (elem "baz" results)
    assertEqual "Unxpected number of results" 3 (length results)
    hangup h2
    hangup h3
    hangup h4
