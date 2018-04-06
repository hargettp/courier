{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric #-}
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
    anyCall,

    methodSelector,
    hear,
    hearTimeout,
    hearAll,
    hearAllTimeout,
    Reply,

    HandleSite(..),
    handle,
    handleAll,
    hangup,

    Request(..),
    RequestId,
    mkRequestId,
    Response(..)

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
import Data.Word

import GHC.Generics hiding (from)

import System.Random

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-|
An identifier for what method to invoke on the receiving 'Endpoint'. If 'hear' has been invoked on the 'Endpoint'
with a matching identifier, then calls will be delivered to that invocation of 'hear'.
-}
type Method = String

data RPCMessageType = Req | Rsp deriving (Eq,Show,Enum,Generic)

instance Serialize RPCMessageType

{-|
A unique identifier for a 'Request'
-}
newtype RequestId = RequestId (Word32, Word32, Word32, Word32) deriving (Generic,Eq,Show)

instance Serialize RequestId

{-|
Create a new identifier for 'Request's
-}
mkRequestId :: IO RequestId
mkRequestId = do
    w1 <- randomIO
    w2 <- randomIO
    w3 <- randomIO
    w4 <- randomIO
    return $ RequestId (w1, w2, w3, w4)

{-|
Encapsulates the initiating side of a 'call': every invocation of 'call' produces a 'Request' that is sent
to the destination 'Endpoint', where the 'hear'ing side will generate a 'Response' after completing the request'
-}
data Request = Request {
    requestId :: RequestId,
    requestCaller :: Name,
    requestMethod :: Method,
    requestArgs :: Message
} deriving (Eq,Show)

instance Serialize Request where
    put req = do
        put Req
        put $ requestId req
        put $ requestCaller req
        put $ requestMethod req
        put $ requestArgs req
    get = do
        Req <- get
        rid <- get
        caller <- get
        method <- get
        args <- get
        return $ Request rid caller method args
{-|
Encapsulates the completion side of a 'call': every invocation of 'call' produces a 'Request' that is sent
to the destination 'Endpoint', where the 'hear'ing side will generate a 'Response' after completing the request'
-}
data Response = Response {
    responseId :: RequestId,
    responseFrom :: Name,
    responseValue :: Message
} deriving (Eq,Show)

instance Serialize Response where
    put rsp = do
        put Rsp
        put $ responseId rsp
        put $ responseFrom rsp
        put $ responseValue rsp

    get = do
        Rsp <- get
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

The caller will wait until a matching response is received.
-}
call :: CallSite -> Name -> Method -> Message -> IO Message
call (CallSite endpoint from) name method args = do
    rid <- mkRequestId
    let req = Request {requestId = rid,requestCaller = from,requestMethod = method, requestArgs = args}
    sendMessage endpoint name $ encode req
    selectMessage endpoint $ \msg -> do
        case decode msg of
            Left _ -> Nothing
            Right (Response respId _ value) -> do
                if respId == rid
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
callWithTimeout :: CallSite -> Name -> Method -> Int-> Message -> IO  (Maybe Message)
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
gcall :: CallSite -> [Name] -> Method -> Message -> IO (M.Map Name Message)
gcall (CallSite endpoint from) names method args = do
    rid <- mkRequestId
    let req = Request {requestId = rid,requestCaller = from,requestMethod = method, requestArgs = args}
    sendAll req
    recvAll req M.empty
    where
        sendAll req = do
            forM_ names $ \name -> sendMessage endpoint name $ encode req
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
gcallWithTimeout :: CallSite -> [Name] -> Method -> Int -> Message -> IO (M.Map Name (Maybe Message))
gcallWithTimeout (CallSite endpoint from) names method delay args = do
    rid <- mkRequestId
    let req = Request {requestId = rid,requestCaller = from,requestMethod = method, requestArgs = args}
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
            forM_ names $ \name -> sendMessage endpoint name $ encode req
        recv req = selectMessage endpoint $ \msg -> do
                case decode msg of
                    Left _ -> Nothing
                    Right (Response rid name value) -> do
                        if (rid == (requestId req)) && (elem name names)
                            then Just (name,value)
                            else Nothing
        recvAll :: Request -> TVar (M.Map Name Message) -> IO (M.Map Name Message)
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
        complete :: M.Map Name b -> M.Map Name (Maybe b)
        complete partial = foldl (\final name -> M.insert name (M.lookup name partial) final) M.empty names

{-|
Invoke the same method on multiple 'Name's, and wait indefinitely until
the first response from any 'Name', returning the value and the 'Name'
which responded.
-}
anyCall :: CallSite -> [Name] -> Method -> Message -> IO (Message,Name)
anyCall (CallSite endpoint from) names method args = do
    rid <- mkRequestId
    let req = Request {requestId = rid,requestCaller = from,requestMethod = method, requestArgs = args}
    sendAll req
    recvAny req
    where
        sendAll req = do
            forM_ names $ \name -> sendMessage endpoint name $ encode req
        recvAny req = selectMessage endpoint $ \msg -> do
                case decode msg of
                    Left _ -> Nothing
                    Right (Response rid name value) -> do
                        if (rid == (requestId req)) && (elem name names)
                            then Just (value,name)
                            else Nothing

{-|
A 'Reply' is a one-shot function for sending a response to an incoming request.
-}
type Reply b = b -> IO ()

{-|
A simple function that, given a 'Method', returns a filter suitable for
use with 'selectMessage'. The typical use case will involve partial
application: @methodSelector method@ passed as an argument to 'selectMessage'.
-}
methodSelector :: Method -> Message -> Maybe (Name,RequestId,Message)
methodSelector method msg = do
    case decode msg of
        Left _ -> Nothing
        Right (Request rid caller rmethod args) -> do
            if rmethod == method
                then Just (caller,rid,args)
                else Nothing

anySelector :: Message -> Maybe (Name,RequestId,Method,Message)
anySelector msg =
  case decode msg of
    Left _ -> Nothing
    Right (Request rid caller method args) -> Just (caller,rid,method,args)

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
hear :: Endpoint -> Name -> Method -> IO (Message,Reply Message)
hear endpoint name method = do
    (caller,rid,args) <- selectMessage endpoint $ methodSelector method
    return (args, reply caller rid)
    where
        reply caller rid result = do
            sendMessage endpoint caller $ encode $ Response rid name result

{-|
Same as 'hear', except return 'Nothing' if no request received within the specified
timeout (measured in microseconds), or return a 'Just' instance containing both the
method arguments and a 'Reply' function useful for sending the reply.
-}
hearTimeout :: Endpoint -> Name -> Method -> Int -> IO (Maybe (Message,Reply Message))
hearTimeout endpoint name method timeout = do
    req <- selectMessageTimeout endpoint timeout $ methodSelector method
    case req of
        Just (caller,rid,args) -> return $ Just (args, reply caller rid)
        Nothing -> return Nothing
    where
        reply caller rid result = do
            sendMessage endpoint caller $ encode $ Response rid name result

{-|
A variant of 'hear', except it listens for any incoming RPC request on the specified 'Endpoint'.
-}
hearAll :: Endpoint -> Name -> IO (Method,Message,Reply Message)
hearAll endpoint name = do
    (caller,rid,method,args) <- selectMessage endpoint selectorForAll
    return (method,args,reply caller rid)
    where
        reply caller rid result =
          sendMessage endpoint caller $ encode $ Response rid name result
        selectorForAll msg =
          case decode msg of
            Left _ -> Nothing
            Right (Request rid caller method args) -> Just (caller,rid,method,args)

{-|
A variant of 'hearTimeout', except it listens for any incoming RPC request on the specified 'Endpoint'
-}
hearAllTimeout :: Endpoint -> Name -> Int -> IO (Maybe (Method,Message,Reply Message))
hearAllTimeout endpoint name timeout = do
    req <- selectMessageTimeout endpoint timeout anySelector
    case req of
        Just (caller,rid,method,args) -> return $ Just (method,args, reply caller rid)
        Nothing -> return Nothing
    where
        reply caller rid result = do
            sendMessage endpoint caller $ encode $ Response rid name result

{-|
A 'HandleSite' is a just reference to the actual handler of a specific method.
Mostly for invoking 'hangup' on the handler, once it is no longer needed.
-}
data HandleSite = HandleSite Name (Async ())

{-|
Handle all RPCs to invoke the indicated 'Method' on the specified 'Endpoint',
until 'hangup' is called on the returned 'HandleSite'.
-}
handle :: Endpoint -> Name -> Method -> (Message -> IO Message) -> IO HandleSite
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
Handle all RPCs on the specified 'Endpoint' until 'hangup' is called on the returned 'HandleSite'
-}
handleAll :: Endpoint -> Name -> (Method -> Message -> IO Message) -> IO HandleSite
handleAll endpoint name fn = do
    task <- async handleCall
    return $ HandleSite name task
    where
        handleCall = do
            (method,args,reply) <- hearAll endpoint name
            result <- fn method args
            reply result
            handleCall

{-|
Stop handling incoming RPCs for the indicated 'HandleSite'.
-}
hangup :: HandleSite -> IO ()
hangup (HandleSite _ task) = do
    cancel task
    return ()
