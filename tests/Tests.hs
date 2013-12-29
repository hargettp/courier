module Main where

-- local imports

import Network.Endpoints

-- external imports

import System.Directory
import System.Log.Logger
import System.Log.Handler.Simple

import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit

-- Test modules
import qualified TestMemory as M
import qualified TestTCP as T
import qualified TestUDP as U

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

main :: IO ()
main = do 
  initLogging
  defaultMain tests

initLogging :: IO ()  
initLogging = do
  let logFile = "tests.log"
  exists <- doesFileExist logFile
  if exists 
    then removeFile logFile  
    else return ()
  s <- fileHandler logFile INFO
  updateGlobalLogger rootLoggerName (setLevel ERROR)
  updateGlobalLogger rootLoggerName (addHandler s)

tests :: [Test.Framework.Test]
tests =
  [
    testCase "hunit" (assertBool "HUnit assertion of truth is false" True),
    testCase "endpoints" testEndpoint
  ]
  ++ M.tests
  ++ T.tests
  ++ U.tests

-- Endpoint tests

testEndpoint :: Assertion
testEndpoint = do
  _ <- newEndpoint []
  return ()
  
