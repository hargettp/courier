module Main where

-- local imports

import TestUtils

-- external imports

import System.IO
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Logger

import Test.Framework

-- Test modules
import qualified TestMailbox as MB
import qualified TestMemory as M
import qualified TestRPC as R
import qualified TestTCP as T
import qualified TestUDP as U

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

main :: IO ()
main = do
  initLogging
  ipv6 <- isIPv6Available
  let allTests = tests ++ T.tests4 ++ U.tests4
  if ipv6
    then defaultMain (allTests ++ T.tests6 ++ U.tests6)
    else defaultMain allTests

initLogging :: IO ()
initLogging = do
  s <- streamHandler stdout INFO
  let fs = setFormatter s $ simpleLogFormatter "$time [$prio] - $msg"
  updateGlobalLogger rootLoggerName (setLevel WARNING)
  updateGlobalLogger rootLoggerName $ setHandlers [fs]

tests :: [Test.Framework.Test]
tests =
  MB.tests
  ++ M.tests
  ++ R.tests
