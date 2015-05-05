module TestTransports (
    testDelay,
    verifiedSend,
    whenIPv6,

    -- Common tests
    endpointTransport,
    endpointBindUnbind,
    endpointSendReceive,
    endpointDoubleSendReceive,
    endpointSendReceiveReply,
    endpointMultipleServerSendReceiveReply,
    endpointLocalSendReceiveReply,
    endpointMultipleSendReceiveReply
) where

-- local imports

import Network.Endpoints

import Network.Transport.TCP

-- external imports

import Control.Concurrent
import Control.Exception

import Data.Serialize

import qualified Network.Socket as NS

import System.Log.Logger

import Test.HUnit

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

_log :: String
_log = "test.transports"

testDelay :: Int
testDelay = 1 * 1000000

pause :: IO ()
pause = threadDelay testDelay

whenIPv6 :: Assertion -> Assertion
whenIPv6 assn = do
    addresses <- lookupAddresses NS.AF_INET6 NS.Stream "localhost:1"
    case addresses of
        [] -> do
            warningM _log $ "IPv6 not available"
            return ()
        _ -> assn

{-
Common tests--just supply the transport factory and an address generator
-}

endpointTransport :: String -> (Resolver -> IO Transport) -> IO Address -> Assertion
endpointTransport _log newTransport newAddress = do
  address1 <- newAddress
  address2 <- newAddress
  let name1 = "endpoint1"
      name2 = "endpoint2"
  let resolver = resolverFromList [(name1,address1),
                               (name2,address2)]
  bracket (newTransport resolver)
          shutdown
          (\transport -> do 
            _ <- newEndpoint [transport]
            return ())

endpointBindUnbind :: String -> (Resolver -> IO Transport) -> IO Address -> Assertion
endpointBindUnbind _log newTransport newAddress = do
  infoM _log "Starting bind-unbind test"
  address1 <- newAddress
  address2 <- newAddress
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

endpointSendReceive :: String -> (Resolver -> IO Transport) -> IO Address -> Assertion
endpointSendReceive _log newTransport newAddress = do
  infoM _log "Starting send-receive test"
  address1 <- newAddress
  address2 <- newAddress
  let name1 = "endpoint1"
      name2 = "endpoint2"
  let resolver = resolverFromList [(name1,address1),
                               (name2,address2)]
  bracketTest _log resolver newTransport $ \transport1 transport2 -> do 
      endpoint1 <- newEndpoint [transport1] 
      endpoint2 <- newEndpoint [transport2]
      Right () <- bindEndpoint endpoint1 name1
      Right () <- bindEndpoint endpoint2 name2
      pause

      verifiedSend _log endpoint1 endpoint2 name1 name2 "hello"

      Right () <- unbindEndpoint endpoint1 name1
      Right () <- unbindEndpoint endpoint2 name2
      return ()
  infoM _log "Finished send-receive test"

endpointDoubleSendReceive :: String -> (Resolver -> IO Transport) -> IO Address -> Assertion
endpointDoubleSendReceive _log newTransport newAddress = do
  infoM _log "Starting double-send-receive test"
  address1 <- newAddress
  address2 <- newAddress
  let name1 = "endpoint1"
      name2 = "endpoint2"
      name3 = "endpoint3"
  let resolver = resolverFromList [(name1,address1),
                               (name2,address2),
                               (name3,address1)]
  bracketTest _log resolver newTransport $ \transport1 transport2 -> do 
      endpoint1 <- newEndpoint [transport1]
      endpoint2 <- newEndpoint [transport2]
      endpoint3 <- newEndpoint [transport1]
      Right () <- bindEndpoint endpoint1 name1
      Right () <- bindEndpoint endpoint2 name2
      Right () <- bindEndpoint endpoint3 name3
      pause

      verifiedSend _log endpoint1 endpoint2 name1 name2 "hello"
      verifiedSend _log endpoint3 endpoint2 name3 name2 "ciao!"

      Right () <- unbindEndpoint endpoint1 name1

      verifiedSend _log endpoint3 endpoint2 name3 name2 "hi!"

      Right () <- unbindEndpoint endpoint2 name2
      Right () <- unbindEndpoint endpoint3 name3
      return ()
  infoM _log "Finished double-send-receive test"

endpointMultipleServerSendReceiveReply :: String -> (Resolver -> IO Transport) -> IO Address -> Assertion
endpointMultipleServerSendReceiveReply _log newTransport newAddress = do
  infoM _log "Starting send-receive-reply test"
  address1 <- newAddress
  address2 <- newAddress
  let name1 = "endpoint1"
      name2 = "endpoint2"
  let resolver = resolverFromList [(name1,address1),
                                   (name2,address2)]
  bracket 
    (newTransport resolver)
    shutdown $ 
    \transport1 -> do
        endpoint1 <- newEndpoint [transport1]
        Right () <- bindEndpoint endpoint1 name1
        pause
        bracket 
            (newTransport resolver)
            shutdown $ 
            \transport2 -> do
                endpoint2 <- newEndpoint [transport2]
                Right () <- bindEndpoint endpoint2 name2
                pause
                
                infoM _log "Testing with client 1 started"

                verifiedSend _log endpoint1 endpoint2 name1 name2 "hello"
                verifiedSend _log endpoint2 endpoint1 name2 name1 "hi!"

                infoM _log "Testing with client 1 finished"
                
        pause
        bracket 
            (newTransport resolver)
            shutdown 
            $ \transport2 -> do
                endpoint2 <- newEndpoint [transport2]
                Right () <- bindEndpoint endpoint2 name2
                pause
                
                infoM _log "Testing with client 2 started"

                verifiedSend _log endpoint1 endpoint2 name1 name2 "hello"
                verifiedSend _log endpoint2 endpoint1 name2 name1 "hi!"

                infoM _log "Testing with client 2 finished"
                
    
  bracketTest _log resolver newTransport $ \transport1 transport2 -> do 
      endpoint1 <- newEndpoint [transport1]
      endpoint2 <- newEndpoint [transport2]
      Right () <- bindEndpoint endpoint1 name1
      Right () <- bindEndpoint endpoint2 name2
      pause

      verifiedSend _log endpoint1 endpoint2 name1 name2 "hello"
      verifiedSend _log endpoint2 endpoint1 name2 name1 "hi!"

      Right () <- unbindEndpoint endpoint1 name1
      Right () <- unbindEndpoint endpoint2 name2

      return ()
  infoM _log "Finished send-receive-reply test"

endpointSendReceiveReply :: String -> (Resolver -> IO Transport) -> IO Address -> Assertion
endpointSendReceiveReply _log newTransport newAddress = do
  infoM _log "Starting send-receive-reply test"
  address1 <- newAddress
  address2 <- newAddress
  let name1 = "endpoint1"
      name2 = "endpoint2"
  let resolver = resolverFromList [(name1,address1),
                                   (name2,address2)]
  bracketTest _log resolver newTransport $ \transport1 transport2 -> do 
      endpoint1 <- newEndpoint [transport1]
      endpoint2 <- newEndpoint [transport2]
      Right () <- bindEndpoint endpoint1 name1
      Right () <- bindEndpoint endpoint2 name2
      pause

      verifiedSend _log endpoint1 endpoint2 name1 name2 "hello"
      verifiedSend _log endpoint2 endpoint1 name2 name1 "hi!"

      Right () <- unbindEndpoint endpoint1 name1
      Right () <- unbindEndpoint endpoint2 name2

      return ()
  infoM _log "Finished send-receive-reply test"

endpointLocalSendReceiveReply :: String -> (Resolver -> IO Transport) -> IO Address -> Assertion
endpointLocalSendReceiveReply _log newTransport newAddress = do
  infoM _log "Starting local-send-receive-reply test"
  address1 <- newAddress
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
        pause

        verifiedSend _log endpoint1 endpoint2 name1 name2 "hello"
        verifiedSend _log endpoint2 endpoint1 name2 name1 "hi!"

        Right () <- unbindEndpoint endpoint1 name1
        Right () <- unbindEndpoint endpoint2 name2

        return ())
  infoM _log "Finished local-send-receive-reply test"

endpointMultipleSendReceiveReply :: String -> (Resolver -> IO Transport) -> IO Address -> Assertion
endpointMultipleSendReceiveReply _log newTransport newAddress = do
  infoM _log "Starting multiple-send-receive-reply test"
  address1 <- newAddress
  address2 <- newAddress
  let name1 = "endpoint1"
      name2 = "endpoint2"
  let resolver = resolverFromList [(name1,address1),
                                   (name2,address2)]
  bracketTest _log resolver newTransport $ \transport1 transport2 -> do
      endpoint1 <- newEndpoint [transport1]
      endpoint2 <- newEndpoint [transport2]
      Right () <- bindEndpoint endpoint1 name1
      Right () <- bindEndpoint endpoint2 name2
      pause

      roundtrip endpoint1 endpoint2 name1 name2
      roundtrip endpoint2 endpoint1 name2 name1
      roundtrip endpoint1 endpoint2 name1 name2
      roundtrip endpoint2 endpoint1 name2 name1

      Right () <- unbindEndpoint endpoint1 name1
      Right () <- unbindEndpoint endpoint2 name2

      return ()
  infoM _log "Finished multiple-send-receive-reply test"
    where
      roundtrip endpoint1 endpoint2 name1 name2 = do
        verifiedSend _log endpoint1 endpoint2 name1 name2 "hello"
        verifiedSend _log endpoint2 endpoint1 name2 name1 "hi!"

-- helpers

bracketTest :: String -> Resolver -> (Resolver -> IO Transport) -> (Transport -> Transport -> Assertion) -> Assertion
bracketTest _log resolver newTransport blk = do
    bracket (newTransport resolver)
        shutdown
        (\transport1 -> 
            bracket (newTransport resolver)
                shutdown
                (\transport2 -> catch (blk transport1 transport2)
                                    (\e -> do 
                                        errorM _log $ "Encountered error running test: " ++ (show (e :: SomeException))
                                        assertFailure "Unexpected error")))

verifiedSend :: String -> Endpoint -> Endpoint -> Name -> Name -> String -> Assertion
verifiedSend _log endpoint1 endpoint2 name1 name2 msg = do
  infoM _log $ "Sending message from " ++ name1 ++ " to " ++ name2
  sendMessage_ endpoint1 name2 $ encode msg
  maybeMsg <- receiveMessageTimeout endpoint2 testDelay
  case maybeMsg of
    Nothing -> assertFailure "No message received"
    Just msg1 -> assertEqual "Received message not same as sent" (Right msg) (decode msg1)
