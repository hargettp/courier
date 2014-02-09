{-# LANGUAGE ExistentialQuantification #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.RPC
-- Copyright   :  (c) Phil Hargett 2014
-- License     :  MIT (see LICENSE file)
--
-- Maintainer  :  phil@haphazardhouse.net
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- An implementation of synchronous remote procedure calls
-- (<http://en.wikipedia.org/wiki/Remote_procedure_call RPC>) on top of
-- 'Network.Endpoints.Endpoint's.
--
-- Applications exporting services for use by other applications via
-- RPC call 'handle' to start listening for incoming RPC requests
-- for a specific 'Method'.  If multiple functions or 'Method's are exported,
-- then separate calls to 'handle' are necessary, one for each exported 'Method'.
-- Each call to 'handle' produces a 'HandleSite' which may be used to terminate
-- future handling of RPCs for that specific method by calling 'hangup' on the
-- returned 'HandleSite'.
--
-- Applications wishing to make RPCs to other applications or services do so
-- by first constructing a 'CallSite', and then 'call'ing specific methods
-- on the target handler through that 'CallSite'.
--
-- Both single and multiple target RPCs are available, as are variants that
-- either wait indefinitely or at most for a defined timeout.
--
-----------------------------------------------------------------------------

module Network.RPC (

    Method,

    newCallSite,
    CallSite,

    call,
    callWithTimeout,
    gcall,
    gcallWithTimeout,

    hear,
    hearTimeout,
    Reply,

    HandleSite,
    handle,
    hangup

) where

-- local imports

import Network.Endpoints

-- external imports

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Serialize
import Data.UUID
import Data.UUID.V4
import Data.Word

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

type Method = String

type RequestId = (Word32, Word32, Word32, Word32)

data Request a = (Serialize a) => Request {
    requestId :: RequestId,
    requestCaller :: Name,
    requestMethod :: Method,
    requestArgs :: a
}

instance (Serialize a) => Serialize (Request a) where 
    put req = do
        put $ requestId req
        put $ requestCaller req
        put $ requestMethod req
        put $ requestArgs req
    get = do
        rid <- get
        caller <- get
        method <- get
        args <- get
        return $ Request rid caller method args

data Response b = (Serialize b) => Response {
    responseId :: RequestId,
    responseFrom :: Name,
    responseValue :: b
}

instance (Serialize b) => Serialize (Response b) where
    put rsp = do
        put $ responseId rsp
        put $ responseFrom rsp
        put $ responseValue rsp

    get = do
        rid <- get
        from <- get
        val <- get
        return $ Response rid from val

{-|
A call site is a location for making RPCs: it includes an endpoint
and a name by which recipients can return the call
-}
data CallSite = CallSite Endpoint Name

{-|
Create a new 'CallSite' using the indicated 'Endpoint' for sending
RPCs and using the specified 'Name' for receiving responses.
-}
newCallSite :: Endpoint -> Name -> CallSite
newCallSite = CallSite

{-|
Call a method with the provided arguments on the recipient with the given name.
A request will be made through the 'CallSite''s 'Endpoint', and then
the caller will wait until a matching response is received.
-}
call :: (Serialize a, Serialize b) => CallSite -> Name -> Method -> a -> IO  b
call (CallSite endpoint from) name method args = do
    ruuid <- nextRandom
    let req = Request {requestId = toWords ruuid,requestCaller = from,requestMethod = method, requestArgs = args}
    sendMessage_ endpoint name $ encode req
    selectMessage endpoint $ \msg -> do
        case decode msg of
            Left _ -> Nothing
            Right (Response rid _ value) -> do
                if rid == (requestId req)
                    then Just value
                    else Nothing

{-|
Call a method with the provided arguments on the recipient with the given name.
A request will be made through the 'CallSite''s 'Endpoint', and then
the caller will wait until a matching response is received. If a response
is received within the provided timeout (measured in microseconds), then
return the value wrapped in 'Just'; otherwise, if the timeout expires
before the call returns, then return 'Nothing.
-}
callWithTimeout :: (Serialize a, Serialize b) => CallSite -> Name -> Method -> Int-> a -> IO  (Maybe b)
callWithTimeout site name method delay args = do
    resultOrTimeout <- race callIt (threadDelay delay)
    case resultOrTimeout of
        Left value -> return $ Just value
        Right _ -> return Nothing
    where
        callIt = call site name method args

{-|
Group call or RPC: call a method with the provided arguments on all the recipients with the given names.
A request will be made through the 'CallSite''s 'Endpoint', and then
the caller will wait until all matching responses are received.
-}
gcall :: (Serialize a, Serialize b) => CallSite -> [Name] -> Method -> a -> IO (M.Map Name b)
gcall (CallSite endpoint from) names method args = do
    ruuid <- nextRandom
    let req = Request {requestId = toWords ruuid,requestCaller = from,requestMethod = method, requestArgs = args}
    sendAll req
    recvAll req M.empty
    where
        sendAll req = do
            forM_ names $ \name -> sendMessage_ endpoint name $ encode req
        recv req = selectMessage endpoint $ \msg -> do
                case decode msg of
                    Left _ -> Nothing
                    Right (Response rid name value) -> do
                        if (rid == (requestId req)) && (elem name names)
                            then Just (name,value)
                            else Nothing
        recvAll req results = do
            (replier,result) <- recv req
            let newResults = M.insert replier result results
                replied = S.fromList $ M.keys newResults
                expected = S.fromList names
            if S.null (S.difference expected replied)
                -- we have everything
                then return newResults
                -- still waiting on some results, so keep receiving
                else recvAll req newResults

{-|
Group call or RPC but with a timeout: call a method with the provided arguments on all the
recipients with the given names. A request will be made through the 'CallSite''s 'Endpoint',
and then the caller will wait until all matching responses are received or the timeout occurs.
The returned 'M.Map' has a key for every 'Name' that was a target of the call, and the value
of that key will be @Nothing@ if no response was received before the timeout, or @Just value@
if a response was received.
-}
gcallWithTimeout :: (Serialize a, Serialize b) => CallSite -> [Name] -> Method -> Int -> a -> IO (M.Map Name (Maybe b))
gcallWithTimeout (CallSite endpoint from) names method delay args = do
    ruuid <- nextRandom
    let req = Request {requestId = toWords ruuid,requestCaller = from,requestMethod = method, requestArgs = args}
    sendAll req
    allResults <- atomically $ newTVar M.empty
    responses <- race (recvAll req allResults) (threadDelay delay)
    case responses of
        Left results -> return $ complete results
        Right _ -> do
            partialResults <- atomically $ readTVar allResults
            return $ complete partialResults
    where
        sendAll req = do
            forM_ names $ \name -> sendMessage_ endpoint name $ encode req
        recv req = selectMessage endpoint $ \msg -> do
                case decode msg of
                    Left _ -> Nothing
                    Right (Response rid name value) -> do
                        if (rid == (requestId req)) && (elem name names)
                            then Just (name,value)
                            else Nothing
        recvAll :: (Serialize b) => Request a -> TVar (M.Map Name b) -> IO (M.Map Name b)
        recvAll req allResults = do
            (replier,result) <- recv req
            newResults <- atomically $ do
                modifyTVar allResults $ \results -> M.insert replier result results
                readTVar allResults
            let replied = S.fromList $ M.keys newResults
                expected = S.fromList names
            if S.null (S.difference expected replied)
                -- we have everything
                then return newResults
                -- still waiting on some results, so keep receiving
                else recvAll req allResults
        -- Make sure the final results have an entry for every name,
        -- but put Nothing for those handlers that did not return a result in time
        complete :: (Serialize b) => M.Map Name b -> M.Map Name (Maybe b)
        complete partial = foldl (\final name -> M.insert name (M.lookup name partial) final) M.empty names

{-|
A 'Reply' is a one-shot function for sending a response to an incoming request.
-}
type Reply b = b -> IO ()

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
hear :: (Serialize a,Serialize b) => Endpoint -> Name -> Method -> IO (a,Reply b)
hear endpoint name method = do
    (caller,rid,args) <- selectMessage endpoint $ \msg -> do
                case decode msg of
                    Left _ -> Nothing
                    Right (Request rid caller rmethod args) -> do
                        if rmethod == method
                            then Just (caller,rid,args)
                            else Nothing
    return (args, reply caller rid)
    where
        reply caller rid result = do
            sendMessage_ endpoint caller $ encode $ Response rid name result


{-|
Same as 'hear', except return 'Nothing' if no request received within the specified
timeout (measured in microseconds), or return a 'Just' instance containing both the
method arguments and a 'Reply' function useful for sending the reply.
-}
hearTimeout :: (Serialize a,Serialize b) => Endpoint -> Name -> Method -> Int -> IO (Maybe (a,Reply b))
hearTimeout endpoint name method timeout = do
    req <- selectMessageTimeout endpoint timeout $ \msg -> do
                case decode msg of
                    Left _ -> Nothing
                    Right (Request rid caller rmethod args) -> do
                        if rmethod == method
                            then Just (caller,rid,args)
                            else Nothing
    case req of
        Just (caller,rid,args) -> return $ Just (args, reply caller rid)
        Nothing -> return Nothing
    where
        reply caller rid result = do
            sendMessage_ endpoint caller $ encode $ Response rid name result

{-|
A 'HandleSite' is a just reference to the actual handler of a specific method.
Mostly for invoking 'hangup' on the handler, once it is no longer needed.
-}
data HandleSite = HandleSite Name (Async ())

{-|
Handle all RPCs to invoke the indicated 'Method' on the specified 'Endpoint',
until 'hangup' is called on the returned 'HandleSite'.
-}
handle :: (Serialize a, Serialize b) => Endpoint -> Name -> Method -> (a -> IO b) -> IO HandleSite
handle endpoint name method fn = do
    task <- async $ handleCall
    return $ HandleSite name task
    where
        handleCall = do
            (args,reply) <- hear endpoint name method
            result <- fn args
            reply result
            handleCall

{-|
Stop handling incoming RPCs for the indicated 'HandleSite'.
-}
hangup :: HandleSite -> IO ()
hangup (HandleSite _ task) = do
    cancel task
    return ()
