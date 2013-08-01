module Main where

-- local imports

import Network.Endpoints

-- external imports

import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

-- Test modules
import qualified TestMemory as M
import qualified TestTCP as T

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: [Test.Framework.Test]
tests = 
  [
    testCase "hunit" (assertBool "HUnit assertion of truth is false" True),
    testProperty "quickcheck" True,
    testCase "endpoints" testEndpoint
  ] 
  ++ M.tests
  ++ T.tests
  -- ++ D.tests
  -- ++ S.tests

-- Endpoint tests

testEndpoint :: Assertion
testEndpoint = do
  _ <- newEndpoint []
  return ()
  
