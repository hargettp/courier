-----------------------------------------------------------------------------
-- |
-- Module      :  TestMailbox
-- Copyright   :  (c) Phil Hargett 2014
-- License     :  MIT (see LICENSE file)
-- 
-- Maintainer  :  phil@haphazardhouse.net
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- (..... module description .....)
--
-----------------------------------------------------------------------------

module TestMailbox (tests) where

-- local imports

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Mailbox
import Control.Concurrent.STM

-- external imports

import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

tests :: [Test.Framework.Test]
tests = [
    testCase "mbox-creation" testCreation,
    testCase "mbox-simple-write" testSimpleWrite,
    testCase "mbox-write-read" testWriteRead,
    testCase "mbox-match" testMatch,
    testCase "mbox-find" testFind
  ]

testCreation :: Assertion
testCreation = do
    _ <- atomically $ newMailbox
    return ()

testSimpleWrite :: Assertion
testSimpleWrite = do
    mbox <- atomically $ newMailbox
    atomically $ writeMailbox mbox "hello!"
    return ()

testWriteRead :: Assertion
testWriteRead = do
    mbox <- atomically $ newMailbox
    atomically $ writeMailbox mbox "hello!"
    results <- race (atomically $ readMailbox mbox)
        (threadDelay 1000000)
    case results of
        Left msg -> assertEqual "Read message did not match written message" msg "hello!"
        Right _ -> assertFailure "No message read"

testMatch :: Assertion
testMatch = do
    mbox <- atomically $ newMailbox
    atomically $ writeMailbox mbox "hello!"
    results1 <- race (atomically $ expectHello mbox)
        (atomically $ expectBonjour mbox)
    case results1 of
        Left _ -> return ()
        Right _ -> assertFailure "Did not receive expected message"
    atomically $ writeMailbox mbox "bonjour!"
    results2 <- race (atomically $ expectHello mbox)
        (atomically $ expectBonjour mbox)
    case results2 of
        Left _ -> assertFailure "Did not receive expected message"
        Right _ -> return ()
    where
        expectHello mbox = selectMailbox mbox (\msg ->  if msg == "hello!" then Just msg else Nothing)
        expectBonjour mbox = selectMailbox mbox (\msg ->  if msg == "bonjour!" then Just msg else Nothing)

testFind :: Assertion
testFind = do
    mbox <- atomically $ newMailbox
    atomically $ writeMailbox mbox "hello!"
    results1 <- race (atomically $ expectHello mbox)
        (threadDelay 1000000)
    case results1 of
        Left _ -> return ()
        Right _ -> assertFailure "Did not find expected message"
    atomically $ writeMailbox mbox "bonjour!"
    results2 <- race (atomically $ expectHello mbox)
        (threadDelay 1000000)
    case results2 of
        Left _ -> return ()
        Right _ -> assertFailure "Did not find expected message"
    results3 <- atomically $ useHello mbox
    assertBool "Expected hello!" (results3 == "hello!")
    results4 <- race (atomically $ expectHello mbox)
        (atomically $ expectBonjour mbox)
    case results4 of
        Left _ -> assertFailure "Did not receive expected message"
        Right _ -> return ()
    where
        useHello mbox = selectMailbox mbox (\msg ->  if msg == "hello!" then Just msg else Nothing)
        expectHello mbox = findMailbox mbox (\msg ->  if msg == "hello!" then Just msg else Nothing)
        expectBonjour mbox = findMailbox mbox (\msg ->  if msg == "bonjour!" then Just msg else Nothing)
