-----------------------------------------------------------------------------
-- |
-- Module      :  TestRPCTyped
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
module TestRPCTyped (
    tests
) where

-- local imports

import Network.Endpoints
import Network.RPC.Typed
import Network.Transport.Memory

import TestUtils

-- external imports

import Control.Concurrent

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
    testCase "call-typed-one-with-timeout" testOneHandlerWithTimeout,
    testCase "gcall-typed-three-handlers-with-timeout"testGroupCallWithTimeout
  ]

testOneHandlerWithTimeout :: Assertion
testOneHandlerWithTimeout = do
  let name1 = Name "endpoint1"
      name2 = Name "endpoint2"
      longer = (3 * 1000000 :: Int)
      shorter = (1 * 1000000 :: Int)
  withTransport newMemoryTransport $ \transport ->
    withNewEndpoint2 transport $ \endpoint1 endpoint2 ->
      withBinding transport endpoint1 name1 $
        withBinding transport endpoint2 name2 $
          withConnection transport endpoint1 name2 $ do
            -- first call with caller waiting longer than handler
            h1 <- handle endpoint2 name2 "foo" $ \msg -> do
                threadDelay shorter
                return $ msg ++ "!"
            let cs1 = newCallSite endpoint1 name1
            Just result1 <- callWithTimeout cs1 name2 "foo" longer "hello"
            assertEqual "Result not expected value" (Just "hello!") (Just result1)
            hangup h1
            -- now call with handler waiting longer than caller
            h2 <- handle endpoint2 name2 "foo" $ \msg -> do
                threadDelay longer
                return $ msg ++ "!"
            let cs2 = newCallSite endpoint1 name1
            result2 <- (callWithTimeout cs2 name2 "foo" shorter "hello")
            assertEqual "Result not expected value" Nothing (result2 :: Maybe String)
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
  withTransport newMemoryTransport $ \transport ->
    withNewEndpoint4 transport $ \endpoint1 endpoint2 endpoint3 endpoint4 -> do
      withBinding4 transport (endpoint1,name1) (endpoint2,name2) (endpoint3,name3) (endpoint4,name4) $
        withConnection3 transport endpoint1 name2 name3 name4 $ do
          h2 <- handle endpoint2 name2 "foo" $ \msg -> do
              threadDelay shorter
              return $ if msg == "hello" then "foo" else ""
          h3 <- handle endpoint3 name3 "foo" $ \msg ->  do
              threadDelay shorter
              return $ if msg == "hello" then "bar" else ""
          h4 <- handle endpoint4 name4 "foo" $ \msg ->  do
              threadDelay longest
              return $ if msg == "hello" then "baz" else ""
          let cs = newCallSite endpoint1 name1
          results <- gcallWithTimeout cs [name2,name3,name4] "foo" longer "hello"
          assertEqual "Foo not present in results" (Just $ Just "foo") (M.lookup name2 results)
          assertEqual "Bar not present in results" (Just $ Just "bar") (M.lookup name3 results)
          assertEqual "Baz shouldn't be present in results" (Just $ Nothing) (M.lookup name4 results)
          assertEqual "Unxpected number of results" 3 (M.size results)
          hangup h2
          hangup h3
          hangup h4
