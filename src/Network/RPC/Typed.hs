{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.RPC.Typed
-- Copyright   :  (c) Phil Hargett 2015
-- License     :  MIT (see LICENSE file)
--
-- Maintainer  :  phil@haphazardhouse.net
-- Stability   :  experimental
-- Portability :  non-portable (uses STM)
--
--
-----------------------------------------------------------------------------

module Network.RPC.Typed (

  call,
  callWithTimeout,
  gcallWithTimeout,
  hear,
  hearTimeout,
  handle,
  typedMethodSelector,

  module Network.RPC

) where

-- local imports

import Network.Endpoints
import Network.RPC hiding (call,callWithTimeout,gcallWithTimeout,hear,hearTimeout,handle)
import qualified Network.RPC as R

-- external imports

import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.Map as M
import Data.Serialize

-- import Debug.Trace

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-|
Call a method with the provided arguments on the recipient with the given name.

The caller will wait until a matching response is received.
-}
call :: (Serialize a,Serialize b) => CallSite -> Name -> Method -> a -> IO b
call cs name method args = do
  result <- R.call cs name method $ encode args
  let Right value = decode result
  return value

{-|
Call a method with the provided arguments on the recipient with the given name.
A request will be made through the 'CallSite''s 'Endpoint', and then
the caller will wait until a matching response is received. If a response
is received within the provided timeout (measured in microseconds), then
return the value wrapped in 'Just'; otherwise, if the timeout expires
before the call returns, then return 'Nothing.
-}
callWithTimeout :: (Serialize a,Serialize r) => CallSite -> Name -> Method -> Int-> a -> IO  (Maybe r)
callWithTimeout site name method delay args = do
    resultOrTimeout <- race callIt (threadDelay delay)
    case resultOrTimeout of
        Left value -> return $ Just value
        Right _ -> return Nothing
    where
        callIt = call site name method args

{-|
Group call or RPC but with a timeout: call a method with the provided arguments on all the
recipients with the given names. A request will be made through the 'CallSite''s 'Endpoint',
and then the caller will wait until all matching responses are received or the timeout occurs.
The returned 'M.Map' has a key for every 'Name' that was a target of the call, and the value
of that key will be @Nothing@ if no response was received before the timeout, or @Just value@
if a response was received.
-}
gcallWithTimeout :: (Serialize a,Serialize r) => CallSite -> [Name] -> Method -> Int -> a -> IO (M.Map Name (Maybe r))
gcallWithTimeout cs names method delay args = do
  responses <- R.gcallWithTimeout cs names method delay $ encode args
  return $ decodeResponses responses
  where
    decodeResponses = M.map decodeResponse
      where
        decodeResponse maybeMsg = case maybeMsg of
          Nothing -> Nothing
          Just msg -> case decode msg of
            Left _ -> Nothing
            Right response -> Just response

{-|
Wait for a single incoming request to invoke the indicated 'Method' on the specified
'Endpoint'. Return both the method arguments and a 'Reply' function useful for sending
the reply.  A good pattern for using 'hear' will pattern match the result to a tuple of
the form @(args,reply)@, then use the args as needed to compute a result, and then
finally send the result back to the client by simply passing the result to reply: @reply result@.

The invoker of 'hear' must supply the 'Name' they have bound to the 'Endpoint', as this
helps the original requestor of the RPC differentiate responses when the RPC was a group
call.
-}
hear :: (Serialize a, Serialize r) => Endpoint -> Name -> Method -> IO (a,Reply r)
hear endpoint name method = do
  (caller,rid,args) <- selectMessage endpoint $ typedMethodSelector method
  return (args, reply caller rid)
  where
    reply caller rid result =
      sendMessage endpoint caller $ encode $ Response rid name $ encode result

{-|
Same as 'hear', except return 'Nothing' if no request received within the specified
timeout (measured in microseconds), or return a 'Just' instance containing both the
method arguments and a 'Reply' function useful for sending the reply.
-}
hearTimeout :: (Serialize a, Serialize r) => Endpoint -> Name -> Method -> Int -> IO (Maybe (a, Reply r))
hearTimeout endpoint name method timeout = do
  -- traceIO $ "listening for " ++ show method ++ " on " ++ show name
  req <- selectMessageTimeout endpoint timeout $ typedMethodSelector method
  case req of
    Just (caller,rid,args) -> do
      -- traceIO $ "heard " ++ show method ++ " on " ++ show name
      return $ Just (args, reply caller rid)
    Nothing -> do
      -- traceIO $ "did not hear " ++ method ++ " on " ++ show name
      return Nothing
  where
  reply caller rid result = sendMessage endpoint caller $ encode $ Response rid name $ encode result

{-|
Handle all RPCs to invoke the indicated 'Method' on the specified 'Endpoint',
until 'hangup' is called on the returned 'HandleSite'.
-}
handle :: (Serialize a, Serialize r) => Endpoint -> Name -> Method -> (a -> IO r) -> IO HandleSite
handle endpoint name method fn = do
    task <- async handleCall
    return $ HandleSite name task
    where
        handleCall = do
            (args,reply) <- hear endpoint name method
            result <- fn args
            reply result
            handleCall

{-|
A method selector that only matches if the message deserializes into
a type that matches arguments to a call.
-}
typedMethodSelector :: (Serialize a) => Method -> Message -> Maybe (Name,RequestId,a)
typedMethodSelector method msg =
  case decode msg of
    Left _ ->
      Nothing
    Right (Request rid caller rmethod bytes) ->
      if rmethod == method
        then case decode bytes of
          Left _ -> Nothing
          Right args -> Just (caller,rid,args)
        else Nothing
