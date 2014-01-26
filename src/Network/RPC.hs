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

    HandleSite,
    handle,
    hangup

) where

-- local imports

import Network.Endpoints

-- external imports

import Control.Concurrent.Async

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
    responseValue :: b
}

instance (Serialize b) => Serialize (Response b) where
    put rsp = do
        put $ responseId rsp
        put $ responseValue rsp

    get = do
        rid <- get
        val <- get
        return $ Response rid val

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
            Right (Response rid value) -> do
                if rid == (requestId req)
                    then Just value
                    else Nothing

{-|
A 'HandleSite' is a just reference to the actual handler of a specific method.
Mostly for invoking 'release' on the handler, once it is no longer needed.
-}
data HandleSite = HandleSite (Async ())

{-|
Handle all RPCs to invoke the indicated 'Method' on the specified 'Endpoint',
until 'hangup' is called.
-}
handle :: (Serialize a, Serialize b) => Endpoint -> Method -> (a -> IO b) -> IO HandleSite
handle endpoint method fn = do
    task <- async $ handleCall
    return $ HandleSite task
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
            sendMessage_ endpoint caller $ encode $ Response rid result
            handleCall

{-|
Stop handling incoming RPCs for the indicated 'HandleSite'.
-}
hangup :: HandleSite -> IO ()
hangup (HandleSite task) = do
    cancel task
    return ()