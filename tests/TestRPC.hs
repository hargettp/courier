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
    testCase "call-foo" testFoo,
    testCase "call-foo-bar" testFooBar
  ]

testFoo :: Assertion
testFoo = do
    let name1 = "endpoint1"
        name2 = "endpoint2"
    transport <- newMemoryTransport
    endpoint1 <- newEndpoint [transport]
    endpoint2 <- newEndpoint [transport]
    Right () <- bindEndpoint endpoint1 name1
    Right () <- bindEndpoint endpoint2 name2
    h <- handle endpoint2 "foo" $ \msg ->
        return $ msg ++ "!"
    let cs = newCallSite endpoint1 name1
    result <- call cs name2 "foo" "hello"
    assertEqual "Result not expected value" "hello!" result
    hangup h

testFooBar :: Assertion
testFooBar = do
    let name1 = "endpoint1"
        name2 = "endpoint2"
    transport <- newMemoryTransport
    endpoint1 <- newEndpoint [transport]
    endpoint2 <- newEndpoint [transport]
    Right () <- bindEndpoint endpoint1 name1
    Right () <- bindEndpoint endpoint2 name2
    h1 <- handle endpoint2 "foo" $ \msg ->
        return $ msg ++ "!"
    h2 <- handle endpoint2 "bar" $ \msg ->
        return $ msg ++ "?"
    let cs = newCallSite endpoint1 name1
    result1 <- call cs name2 "foo" "hello"
    assertEqual "Result not expected value" "hello!" result1
    result2 <- call cs name2 "bar" "hello"
    assertEqual "Result not expected value" "hello?" result2
    hangup h1
    hangup h2
    
