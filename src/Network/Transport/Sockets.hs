{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Transport.Sockets
-- Copyright   :  (c) Phil Hargett 2013
-- License     :  MIT (see LICENSE file)
-- 
-- Maintainer  :  phil@haphazardhouse.net
-- Stability   :  experimental
-- Portability :  non-portable (uses STM)
--
-- Common facilities for socket-based transports, such as UDP and TCP/IP.
--
-----------------------------------------------------------------------------

module Network.Transport.Sockets (

  Connection(..),

  IdentifyMessage(..),
  
  Messenger(..),

  dispatcher,
  receiver,
  receiveMessage,
  SocketSend
  
  ) where

-- local imports

import Network.Transport

-- external imports
import Control.Concurrent.Async
import Control.Concurrent.STM

import Control.Exception

import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Serialize

import GHC.Generics

import Network.Simple.TCP (recv)
import Network.Socket hiding (recv,socket)

import System.Log.Logger

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

_log :: String
_log = "transport.sockets"

data IdentifyMessage = IdentifyMessage Address deriving (Generic)

instance Serialize IdentifyMessage

type SocketSend = Socket -> B.ByteString -> IO ()

data Connection = Connection {
  connSocket :: TMVar Socket,
  connConnect :: IO Socket,
  connSend :: Socket -> B.ByteString -> IO (),
  connReceive :: Socket -> Int -> IO (Maybe B.ByteString)
  }
                  
data Messenger = Messenger {
  messengerOut :: Mailbox,
  messengerAddress :: Address,
  messengerSender :: Async (),
  messengerReceiver :: Async (),
  messengerSocket :: TMVar Socket
  }

instance Show Messenger where
  show msngr = "Messenger(" ++ (show $ messengerAddress msngr) ++ ")"
                 
-- newMessenger :: TMVar Socket -> Address -> Mailbox -> IO Messenger                 
-- newMessenger socket address inc = do
--   out <- newMailbox
--   sndr <- async $ sender socket address out
--   rcvr <- async $ receiver socket address inc
--   return Messenger {
--     messengerOut = out,
--     messengerAddress = address,
--     messengerSender = sndr,
--     messengerReceiver = rcvr,
--     messengerSocket = socket
--     }

dispatcher :: TVar (M.Map Name Mailbox) -> Mailbox -> IO ()
dispatcher bindings mbox = dispatchMessages
  where
    dispatchMessages = catch (do
                                 infoM _log $ "Dispatching messages"
                                 env <- atomically $ readTQueue mbox
                                 dispatchMessage env
                                 dispatchMessages) 
                       (\e -> do 
                           warningM _log $ "Dispatch error: " ++ (show (e :: SomeException)))
    dispatchMessage env = do
      infoM _log $ "Dispatching message"
      let envelopeOrErr = decode env
      case envelopeOrErr of
        Left err -> do
          errorM _log $ "Error decoding message for dispatch: " ++ err
          return ()
        Right (Envelope destination msg) -> do 
          atomically $ do 
            dests <- readTVar bindings
            let maybeDest = M.lookup destination dests
            case maybeDest of
              Nothing -> return ()
              Just dest -> do 
                writeTQueue dest msg
                return ()

receiver :: TMVar Socket -> Address -> Mailbox -> IO ()
receiver socketVar address mailbox  = receiveMessages
  where
    receiveMessages = catch (do
      infoM _log $ "Waiting to receive from " ++ (show address)
      socket <- atomically $ readTMVar socketVar
      maybeMsg <- receiveMessage socket
      infoM _log $ "Received message from " ++ (show address)
      case maybeMsg of
        Nothing -> do
          sClose socket
          return ()
        Just msg -> do
          atomically $ writeTQueue mailbox msg
          receiveMessages) (\e -> do 
                           warningM _log $ "Receive error: " ++ (show (e :: SomeException)))
        
receiveMessage :: Socket -> IO (Maybe Message)    
receiveMessage socket = catch (do
  maybeLen <- recv socket 8 -- TODO must figure out what defines length of an integer in bytes 
  case maybeLen of
    Nothing -> do
      errorM _log $ "No length received"
      return Nothing
    Just len -> do 
      maybeMsg <- recv socket $ msgLength (decode len)
      infoM _log $ "Received message"
      return maybeMsg) (\e -> do 
                  warningM _log $ "Receive error: " ++ (show (e :: SomeException))
                  throw e)
  where
    msgLength (Right size) = size
    msgLength (Left err) = error err
