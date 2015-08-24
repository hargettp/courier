-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.Mailbox
-- Copyright   :  (c) Phil Hargett 2014
-- License     :  MIT (see LICENSE file)
--
-- Maintainer  :  phil@haphazardhouse.net
-- Stability   :  experimental
-- Portability :  non-portable (uses STM)
--
-- A 'Mailbox' is a drop-in replacement for 'TQueue' in "Control.Concurrent.STM", except that
-- it also supports selective out of order message reception: that is, it allows the caller
-- to dequeue the first message among the messages available in the queue that matches
-- a supplied test function, or block if no such match is possible with the messages currently
-- in the queue.
--
-- As 'Mailbox' implements the same basic @read / write / peek@ group of functions as a 'TQueue',
-- it offers a superset of 'TQueue' functionality by extending it with the @find / select / handle@
-- groups of functions.  Thus, applications can safely use 'Mailbox'es in place of 'TQueue's,
-- but choose when to take the slight extra overhead of 'Mailbox' functionality.
--
-- Because message selection in worst case requires fully traversing all messages in the queue,
-- application designers are encouraged to understand this aspect when choosing to use 'Mailbox'es
-- in their designs, or when using the additional features of 'Mailbox'es beyond that of 'TQueue's.
-- Dispatching messages with a 'Mailbox' is analogous to using a @case@ expression (O(n)) to dispatch
-- messages to a handler function, except that new cases can be added or removed at any time.  In essence,
-- one can regard 'Mailbox'es as a useful means of creating an extensible message dispatch function.
-- If, however, if O(1) message dispatching time is necessary or desired, (using hashmaps, for example)
-- then 'Mailbox'es are not the correct choice.
--
-- Despite this extra cost, 'Mailbox'es offer advantages to designers:
--
-- * Implementation of Erlang-style message reception: as messages can be received
-- out of order, a mailbox is analogous to a process input queue in Erlang.
--
-- * Better composability: if applications must only dequeue messages in the order in which
-- they are queued (which is sufficient for many applications), then the main message
-- pump requires modification each time a new class of message must be handled. With
-- selective message reception, multiple concurrent message pumps are possible (with
-- a small performance impact), each processing the messages they expect and with no
-- need to be aware of other message pumps performing their own work on the same mailbox.
--
-- * Mixing synchronous and asynchronous programming styles: if restricted to in order message
-- delivery, an application must carefully construct all logic to avoid blocking its central message
-- loop. By supporting out of message delivery and multiple selective recipients, it becomes possible
-- to combine synchronous and asynchronous programming styles using the same 'Mailbox'.
--
-- Basic framework for 'Mailbox' brazenly copied from "Control.Concurrent.STM.TQueue".
-----------------------------------------------------------------------------

module Control.Concurrent.Mailbox (
    -- * Mailbox
    Mailbox,
    newMailbox,
    newMailboxIO,
    writeMailbox,
    readMailbox,
    tryReadMailbox,
    peekMailbox,
    tryPeekMailbox,
    selectMailbox,
    trySelectMailbox,
    handleMailbox,
    findMailbox,
    tryFindMailbox,
    unGetMailbox,
    isEmptyMailbox,
) where

-- local imports

-- external imports

import Control.Concurrent.STM

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-|
A 'Mailbox' is a variation of a 'Control.Concurrent.TQueue', with the added feature of selective message retrieval.
-}
data Mailbox m = Mailbox
    {-# UNPACK #-} !(TVar [m])
    {-# UNPACK #-} !(TVar [m])

instance Eq (Mailbox m) where
  Mailbox a _ == Mailbox b _ = a == b

{-|
Build and returns a new instance of 'Mailbox'
-}
newMailbox :: STM (Mailbox m)
newMailbox = do
  _read  <- newTVar []
  _write <- newTVar []
  return (Mailbox _read _write)

{-|
@IO@ version of 'newMailbox'.  This is useful for creating top-level
'Mailbox's using 'System.IO.Unsafe.unsafePerformIO', because using
'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
possible.
-}
newMailboxIO :: IO (Mailbox m)
newMailboxIO = do
  _read  <- newTVarIO []
  _write <- newTVarIO []
  return (Mailbox _read _write)

{-|
Write a value to a 'Mailbox'.
-}
writeMailbox :: Mailbox m -> m -> STM ()
writeMailbox (Mailbox _read _write) msg = do
    listend <- readTVar _write
    writeTVar _write (msg:listend)

{-|
Read the next value from the 'Mailbox'.
-}
readMailbox :: Mailbox m -> STM m
readMailbox (Mailbox _read _write) = do
  xs <- readTVar _read
  case xs of
    (x:xs') -> do writeTVar _read xs'
                  return x
    [] -> do ys <- readTVar _write
             case ys of
               [] -> retry
               _  -> case reverse ys of
                       [] -> error "readMailbox"
                       (z:zs) -> do writeTVar _write []
                                    writeTVar _read zs
                                    return z

{-|
A version of 'readMailbox' which does not retry. Instead it
returns @Nothing@ if no value is available.
-}
tryReadMailbox :: Mailbox m -> STM (Maybe m)
tryReadMailbox mailbox = fmap Just (readMailbox mailbox) `orElse` return Nothing

{-|
Get the next value from the @Mailbox@ without removing it,
retrying if the channel is empty.
-}
peekMailbox :: Mailbox m -> STM m
peekMailbox mailbox = do
  msg <- readMailbox mailbox
  unGetMailbox mailbox msg
  return msg

{-|
A version of 'peekMailbox' which does not retry. Instead it
returns @Nothing@ if no value is available.
-}
tryPeekMailbox :: Mailbox m -> STM (Maybe m)
tryPeekMailbox mailbox = do
  maybeMsg <- tryReadMailbox mailbox
  case maybeMsg of
    Nothing -> return Nothing
    Just msg -> do
      unGetMailbox mailbox msg
      return $ Just msg

{-|
Find the next message in the mailbox that matches the supplied test
function or block until there is a message that does. When a message
matches (e.g., test functions returns @Just v@), return it.
-}
selectMailbox :: Mailbox m -> (m -> Maybe v) -> STM v
selectMailbox (Mailbox _read _write) testFn = do
    readMessages <- readTVar _read
    let (maybeReadMsg,newRead) = extract testFn readMessages
    case maybeReadMsg of
        Just msg -> do
            writeTVar _read newRead
            return msg
        Nothing -> do
            writeMessages <- readTVar _write
            let (maybeWriteMsg,newWrite) = extract testFn writeMessages
            case maybeWriteMsg of
                Just msg -> do
                    writeTVar _write newWrite
                    return msg
                Nothing -> retry

{-|
A version of 'selectMailbox' which does not retry. Instead it
returns @Nothing@ if no value is available.
-}
trySelectMailbox :: Mailbox m -> (m -> Maybe v)-> STM (Maybe v)
trySelectMailbox mailbox testFn = fmap Just (selectMailbox mailbox testFn) `orElse` return Nothing

{-|
Wait until there is a message in the mailbox matching the supplied test
function (using `selectMailbox`), then when a message is found, handle
it in the `IO` monad with the supplied function.
-}
handleMailbox :: Mailbox m -> (m -> Maybe v) -> (v -> IO r) -> IO r
handleMailbox mailbox testFn handleFn = do
    selectedMsg <- atomically $ selectMailbox mailbox testFn
    handleFn selectedMsg

{-|
Find the next value from the @Mailbox@ matching @testFn@ without removing it,
retrying if the channel is empty.
-}
findMailbox :: Mailbox m -> (m -> Maybe v) -> STM v
findMailbox (Mailbox _read write) testFn = do
    readMessages <- readTVar _read
    case find testFn readMessages of
        Just msg -> do
            return msg
        Nothing -> do
            writeMessages <- readTVar write
            case find testFn writeMessages of
                Just msg -> do
                    return msg
                Nothing -> retry

{-|
A version of 'findMailbox' which does not retry. Instead it
returns @Nothing@ if no value is available.
-}
tryFindMailbox :: Mailbox m -> (m -> Maybe v) -> STM (Maybe v)
tryFindMailbox (Mailbox _read write) testFn = do
    readMessages <- readTVar _read
    case find testFn readMessages of
        Just msg -> do
            return $ Just msg
        Nothing -> do
            writeMessages <- readTVar write
            case find testFn writeMessages of
                Just msg -> do
                    return $ Just msg
                Nothing -> return Nothing

{-|
Put a data item back onto a mailbox, where it will be the next item read.
-}
unGetMailbox :: Mailbox m -> m -> STM ()
unGetMailbox (Mailbox _read _write) msg = do
  xs <- readTVar _read
  writeTVar _read (msg:xs)

{-|
Returns 'True' if the supplied 'Mailbox' is empty.
-}
isEmptyMailbox :: Mailbox m -> STM Bool
isEmptyMailbox (Mailbox _read write) = do
  xs <- readTVar _read
  case xs of
    (_:_) -> return False
    [] -> do ys <- readTVar write
             case ys of
               [] -> return True
               _  -> return False

--
-- Internal helpers
--

{-|
Extract the first element from a list matching the
provided test and return a new list without the matching
element.
-}
extract :: (m -> Maybe v) -> [m] -> (Maybe v,[m])
extract _ [] = (Nothing,[])
extract test (x:xs) =
    case test x of
        Nothing -> let (result,rest) = extract test xs
                 in (result,x:rest)
        Just v -> (Just v,xs)

{-|
Find the first element from a list matching the
provided test, or Nothing if there is no match.
-}
find :: (m -> Maybe v) -> [m] -> Maybe v
find _ [] = Nothing
find testFn (x:xs) =
    case testFn x of
        Nothing -> find testFn xs
        Just v -> Just v
