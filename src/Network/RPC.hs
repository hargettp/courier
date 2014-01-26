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
-- An implementation of synchronous remote procedure calls on top of
-- 'Network.Endpoints.Endpoint's.
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

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

type Method = String

data Request a = (Serialize a) => Request {
    requestId :: String,
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
    responseId :: String,
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
RPCs and using the specified 'Name' for receiving 'Response'es.
-}
newCallSite :: Endpoint -> Name -> CallSite
newCallSite = CallSite

{-|
Call a method with the provided arguments on the recipient with the given name.
A 'Request' will be made through the 'CallSite''s 'Endpoint', and then
the caller will wait until a matching 'Response' is received.
-}
call :: (Serialize a, Serialize b) => CallSite -> Name -> Method -> a -> IO  b
call (CallSite endpoint from) name method args = do
    let req = Request {requestId = "",requestCaller = from,requestMethod = method, requestArgs = args}
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
A 'Request' will be made through the 'CallSite''s 'Endpoint', and then
the caller will wait until a matching 'Response' is received. If a response
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
A 'Request' will be made through the 'CallSite''s 'Endpoint', and then
the caller will wait until all matching 'Response's are received.
-}
gcall :: (Serialize a, Serialize b) => CallSite -> [Name] -> Method -> a -> IO (M.Map Name b)
gcall (CallSite endpoint from) names method args = do
    let req = Request {requestId = "",requestCaller = from,requestMethod = method, requestArgs = args}
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
recipients with the given names. A 'Request' will be made through the 'CallSite''s 'Endpoint',
and then the caller will wait until all matching 'Response's are received or the timeout occurs.
-}
gcallWithTimeout :: (Serialize a, Serialize b) => CallSite -> [Name] -> Method -> Int -> a -> IO (M.Map Name (Maybe b))
gcallWithTimeout (CallSite endpoint from) names method delay args = do
    let req = Request {requestId = "",requestCaller = from,requestMethod = method, requestArgs = args}
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
A 'HandleSite' is a just reference to the actual handler of a specific method.
Mostly for invoking 'release' on the handler, once it is no longer needed.
-}
data HandleSite = HandleSite Name (Async ())

{-|
Handle all RPCs to invoke the indicated 'Method' on the specified 'Endpoint',
until 'hangup' is called.
-}
handle :: (Serialize a, Serialize b) => Endpoint -> Name -> Method -> (a -> IO b) -> IO HandleSite
handle endpoint name method fn = do
    task <- async $ handleCall
    return $ HandleSite name task
    where
        handleCall = do
            (caller,rid,args) <- selectMessage endpoint $ \msg -> do
                case decode msg of
                    Left _ -> Nothing
                    Right (Request rid caller rmethod args) -> do
                        if rmethod == method
                            then Just (caller,rid,args)
                            else Nothing
            result <- fn args
            sendMessage_ endpoint caller $ encode $ Response rid name result
            handleCall

{-|
Stop handling incoming RPCs for the indicated 'HandleSite'.
-}
hangup :: HandleSite -> IO ()
hangup (HandleSite _ task) = do
    cancel task
    return ()