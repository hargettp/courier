module Main where

-- local imports

import Network.Endpoints

-- external imports

import System.IO
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Logger

import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit

-- Test modules
import qualified TestMailbox as MB
import qualified TestMemory as M
import qualified TestTCP as T
import qualified TestUDP as U
import qualified TestRPC as R

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

main :: IO ()
main = do
  initLogging
  defaultMain tests

initLogging :: IO ()
initLogging = do
  s <- streamHandler stdout INFO
  let fs = setFormatter s $ simpleLogFormatter "$time [$prio] - $msg"
  updateGlobalLogger rootLoggerName (setLevel WARNING)
  updateGlobalLogger rootLoggerName $ setHandlers [fs]

tests :: [Test.Framework.Test]
tests =
  [
    testCase "hunit" (assertBool "HUnit assertion of truth is false" True),
    testCase "endpoints" testEndpoint
  ]
  -- ++ MB.tests
  -- ++ M.tests
  ++ T.tests
  -- ++ U.tests
  -- ++ R.tests

-- Endpoint tests

testEndpoint :: Assertion
testEndpoint = do
  _ <- newEndpoint []
  return ()
