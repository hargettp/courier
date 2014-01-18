module TestCommon (
    testDelay,
    verifiedSend,

    -- Common tests
    endpointTransport,
    endpointBindUnbind,
    endpointSendReceive,
    endpointDoubleSendReceive,
    endpointSendReceiveReply,
    endpointLocalSendReceiveReply,
    endpointMultipleSendReceiveReply
) where

-- local imports

import Network.Endpoints

-- external imports

import Control.Concurrent
import Control.Exception

import Data.Serialize

import System.Log.Logger

import Test.HUnit

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

testDelay :: Int
testDelay = 1 * 1000000

verifiedSend :: String -> Endpoint -> Endpoint -> Name -> Name -> String -> Assertion
verifiedSend _log endpoint1 endpoint2 name1 name2 msg = do
  infoM _log $ "Sending message from " ++ name1 ++ " to " ++ name2
  sendMessage_ endpoint1 name2 $ encode msg
  Just msg1 <- receiveMessageTimeout endpoint2 testDelay
  assertEqual "Received message not same as sent" (Right msg) (decode msg1)

{-
Common tests--just supply the transport factory and 2 addresses
-}

endpointTransport :: String -> (Resolver -> IO Transport) -> Address -> Address -> Assertion
endpointTransport _log newTransport address1 address2 = do
  let name1 = "endpoint1"
      name2 = "endpoint2"
  let resolver = resolverFromList [(name1,address1),
                               (name2,address2)]
  bracket (newTransport resolver)
          shutdown
          (\transport -> do 
            _ <- newEndpoint [transport]
            return ())

endpointBindUnbind :: String -> (Resolver -> IO Transport) -> Address -> Address -> Assertion
endpointBindUnbind _log newTransport address1 address2 = do
  let name1 = "endpoint1"
      name2 = "endpoint2"
  let resolver = resolverFromList [(name1,address1),
                               (name2,address2)]
  bracket (newTransport resolver)
          shutdown
          (\transport -> do 
            endpoint <- newEndpoint [transport]
            Right () <- bindEndpoint endpoint name1
            unbound <- unbindEndpoint endpoint name1
            case unbound of
              Left err -> assertFailure $ "Unbind failed: " ++ err
              Right () -> assertBool "Unbind succeeded" True
            return ())

endpointSendReceive :: String -> (Resolver -> IO Transport) -> Address -> Address -> Assertion
endpointSendReceive _log newTransport address1 address2 = do
  let name1 = "endpoint1"
      name2 = "endpoint2"
  let resolver = resolverFromList [(name1,address1),
                               (name2,address2)]
  bracket (newTransport resolver)
    shutdown
    (\transport1 -> 
        bracket (newTransport resolver)
          shutdown
          (\transport2 -> do 
              endpoint1 <- newEndpoint [transport1]
              endpoint2 <- newEndpoint [transport2]
              Right () <- bindEndpoint endpoint1 name1
              Right () <- bindEndpoint endpoint2 name2
              threadDelay testDelay

              infoM _log "Sending message from 1 to 2"
              sendMessage_ endpoint1 name2 $ encode "hello!"
              Just msg <- receiveMessageTimeout endpoint2 testDelay
              assertEqual "Received message not same as sent" (Right "hello!") (decode msg)
              Right () <- unbindEndpoint endpoint1 name1
              Right () <- unbindEndpoint endpoint2 name2
              return ()))

endpointDoubleSendReceive :: String -> (Resolver -> IO Transport) -> Address -> Address -> Assertion
endpointDoubleSendReceive _log newTransport address1 address2 = do
  let name1 = "endpoint1"
      name2 = "endpoint2"
      name3 = "endpoint3"
  let resolver = resolverFromList [(name1,address1),
                               (name2,address2),
                               (name3,address1)]
  bracket (newTransport resolver)
    shutdown
    (\transport1 -> do 
        bracket (newTransport resolver)
          shutdown
          (\transport2 -> do
              endpoint1 <- newEndpoint [transport1]
              endpoint2 <- newEndpoint [transport2]
              endpoint3 <- newEndpoint [transport1]
              Right () <- bindEndpoint endpoint1 name1
              Right () <- bindEndpoint endpoint2 name2
              Right () <- bindEndpoint endpoint3 name3
              threadDelay testDelay

              verifiedSend _log endpoint1 endpoint2 name1 name2 "hello"
              verifiedSend _log endpoint3 endpoint2 name3 name2 "ciao!"
              
              Right () <- unbindEndpoint endpoint1 name1
              
              verifiedSend _log endpoint3 endpoint2 name3 name2 "hi!"
              
              Right () <- unbindEndpoint endpoint2 name2
              Right () <- unbindEndpoint endpoint3 name3
              return ()))

endpointSendReceiveReply :: String -> (Resolver -> IO Transport) -> Address -> Address -> Assertion
endpointSendReceiveReply _log newTransport address1 address2 = do
  let name1 = "endpoint1"
      name2 = "endpoint2"
  let resolver = resolverFromList [(name1,address1),
                                   (name2,address2)]
  bracket (newTransport resolver)
    shutdown
    (\transport1 -> 
        bracket (newTransport resolver)
          shutdown
          (\transport2 -> do 
              endpoint1 <- newEndpoint [transport1]
              endpoint2 <- newEndpoint [transport2]
              Right () <- bindEndpoint endpoint1 name1
              Right () <- bindEndpoint endpoint2 name2
              threadDelay testDelay
              
              verifiedSend _log endpoint1 endpoint2 name1 name2 "hello"
              verifiedSend _log endpoint2 endpoint1 name2 name1 "hi!"
              
              Right () <- unbindEndpoint endpoint1 name1
              Right () <- unbindEndpoint endpoint2 name2
                
              return ()))
  
endpointLocalSendReceiveReply :: String -> (Resolver -> IO Transport) -> Address -> Address -> Assertion
endpointLocalSendReceiveReply _log newTransport address1 _ = do
  let name1 = "endpoint1"
      name2 = "endpoint2"
  let resolver = resolverFromList [(name1,address1),
                                   (name2,address1)]
  bracket (newTransport resolver)
    shutdown
    (\transport1 -> do
        endpoint1 <- newEndpoint [transport1]
        endpoint2 <- newEndpoint [transport1]
        Right () <- bindEndpoint endpoint1 name1
        Right () <- bindEndpoint endpoint2 name2
        threadDelay testDelay
              
        verifiedSend _log endpoint1 endpoint2 name1 name2 "hello"
        verifiedSend _log endpoint2 endpoint1 name2 name1 "hi!"
        
        Right () <- unbindEndpoint endpoint1 name1
        Right () <- unbindEndpoint endpoint2 name2
                
        return ())
    
endpointMultipleSendReceiveReply :: String -> (Resolver -> IO Transport) -> Address -> Address -> Assertion
endpointMultipleSendReceiveReply _log newTransport address1 address2 = do
  let name1 = "endpoint1"
      name2 = "endpoint2"
  let resolver = resolverFromList [(name1,address1),
                                   (name2,address2)]
  bracket (newTransport resolver)
    shutdown
    (\transport1 -> 
        bracket (newTransport resolver)
          shutdown
          (\transport2 -> do 
              endpoint1 <- newEndpoint [transport1]
              endpoint2 <- newEndpoint [transport2]
              Right () <- bindEndpoint endpoint1 name1
              Right () <- bindEndpoint endpoint2 name2
              threadDelay testDelay

              roundtrip endpoint1 endpoint2 name1 name2
              roundtrip endpoint2 endpoint1 name2 name1
              roundtrip endpoint1 endpoint2 name1 name2
              roundtrip endpoint2 endpoint1 name2 name1
              
              Right () <- unbindEndpoint endpoint1 name1
              Right () <- unbindEndpoint endpoint2 name2
                
              return ()))
    where
      roundtrip endpoint1 endpoint2 name1 name2 = do
        verifiedSend _log endpoint1 endpoint2 name1 name2 "hello"                
        verifiedSend _log endpoint2 endpoint1 name2 name1 "hi!"
