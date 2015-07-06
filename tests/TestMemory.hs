module TestMemory (tests) where

-- local imports

import Network.Endpoints
import Network.Transport.Memory

-- external imports

import Control.Exception

import Data.Serialize

import System.Log.Logger

import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit
-- import Test.Framework.Providers.QuickCheck2
import Text.Printf

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

tests :: [Test.Framework.Test]
tests = [
    testCase "mem-endpoints+transport" testEndpointTransport,
    {-
    testCase "mem-bind" testEndpointBind,
    testCase "mem-unbind" testEndpointBindUnbind,
    -}
    testCase "mem-sendReceive" testEndpointSendReceive,
    testCase "mem-transport" testMemoryTransport
  ]

testEndpointTransport :: Assertion
testEndpointTransport = do
  transport <- newMemoryTransport
  withEndpoint transport $ \_ -> return ()

{-
testEndpointBind :: Assertion
testEndpointBind = do
  let name1 = Name "endpoint1"
  transport <- newMemoryTransport
  finally (do
          endpoint <- newEndpoint transport
          Right () <- bindEndpoint endpoint name1
          return ())
      (shutdown transport)

testEndpointBindUnbind :: Assertion
testEndpointBindUnbind = do
  let name1 = Name "endpoint1"
  transport <- newMemoryTransport
  finally (do
          endpoint <- newEndpoint transport
          Right () <- bindEndpoint endpoint name1
          unbound <- unbindEndpoint endpoint name1
          case unbound of
            Left err -> assertFailure $ "Unbind failed: " ++ err
            Right () -> assertBool "Unbind succeeded" True
          return ())
      (shutdown transport)
-}

_log :: String
_log = "_test_memory"

testEndpointSendReceive :: Assertion
testEndpointSendReceive = do
  let name1 = Name "endpoint1"
      name2 = Name "endpoint2"
  transport <- newMemoryTransport
  infoM _log $ printf "Transport created"
  withEndpoint transport $ \endpoint1 ->
    withEndpoint transport $ \endpoint2 -> do
      infoM _log $ printf "Endpoints created"
      withBinding endpoint1 name1 $ do
        infoM _log $ printf "Bound to %v" (show name1)
        withBinding endpoint2 name2 $ do
          infoM _log $ printf "Bound to %v" (show name2)
          infoM _log $ printf "Sending message from %v to %v" (show name1) (show name2)
          withConnection endpoint1 name2 $ do
            sendMessage endpoint1 name2 $ encode "hello!"
            infoM _log $ printf "Sent message from %v to %v" (show name1) (show name2)
            msg <- receiveMessage endpoint2
            infoM _log $ printf "Received message from %v to %v" (show name1) (show name2)
            assertEqual "Received message not same as sent" (Right "hello!") (decode msg)
            return ()

-- Memory tests

testMemoryTransport :: Assertion
testMemoryTransport = do
    transport <- newMemoryTransport
    shutdown transport
