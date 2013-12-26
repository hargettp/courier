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
  newMessenger,

  dispatcher,
  sender,
  receiver,
  receiveMessage,
  SocketSend,

  parseSocketAddress

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
import qualified Data.Text as T

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

{-|
Parse a TCP 'Address' into its respective 'HostName' and 'PortNumber' components, on the
assumption the 'Address' has an identifer in the format @host:port@. If
the port number is missing from the supplied address, it will default to 0.  If the
hostname component is missing from the identifier (e.g., just @:port@), then hostname
is assumed to be @localhost@.
-}
parseSocketAddress :: Address -> (HostName,ServiceName)
parseSocketAddress address = 
  let identifer = T.pack $ address 
      parts = T.splitOn ":" identifer
  in if (length parts) > 1 then
       (host $ T.unpack $ parts !! 0, port $ T.unpack $ parts !! 1)
     else (host $ T.unpack $ parts !! 0, "0")
  where
    host h = if h == "" then
               "localhost"
             else h
    port p = p

type SocketSend = Socket -> B.ByteString -> IO ()

data Connection = Connection {
  connAddress :: Address,
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
  messengerConnection :: Connection
  }

instance Show Messenger where
  show msngr = "Messenger(" ++ (show $ messengerAddress msngr) ++ ")"

newMessenger :: Connection -> Mailbox -> IO Messenger
newMessenger conn inc = do
  out <- newMailbox
  sndr <- async $ sender conn out
  rcvr <- async $ receiver conn inc
  return Messenger {
    messengerOut = out,
    messengerAddress = connAddress conn,
    messengerSender = sndr,
    messengerReceiver = rcvr,
    messengerConnection = conn
    }

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

sender :: Connection -> Mailbox -> IO ()
sender conn mailbox = sendMessages
  where
    sendMessages = do
      reconnect
      catch (do
                infoM _log $ "Waiting to send to " ++ (show $ connAddress conn)
                msg <- atomically $ readTQueue mailbox
                infoM _log $ "Sending message to " ++ (show $ connAddress conn)
                connected <- atomically $ tryReadTMVar $ connSocket conn
                case connected of
                  Just socket -> do 
                    (connSend conn) socket $ encode (B.length msg)
                    infoM _log $ "Length sent"
                    (connSend conn) socket msg
                    infoM _log $ "Message sent to" ++ (show $ connAddress conn)
                  Nothing -> return ()
            ) (\e -> do
                  warningM _log $ "Send error: " ++ (show (e :: SomeException))
                  disconnect)
      sendMessages
    reconnect = do
      -- TODO need a timeout here, in case connecting always fails
      connected <- atomically $ tryReadTMVar $ connSocket conn
      case connected of
        Just _ -> return ()
        Nothing -> do
          let (host,port) = parseSocketAddress $ connAddress conn
          infoM _log $ "Connecting to " ++ (show host) ++ ":" ++ (show port) -- (show address)
          socket <- connConnect conn
          infoM _log $ "Connected to " ++ (show $ connAddress conn)
          atomically $ putTMVar (connSocket conn) socket
    disconnect = do
      connected <- atomically $ tryTakeTMVar $ connSocket conn
      case connected of
        Just socket -> sClose socket
        Nothing -> return ()

receiver :: Connection -> Mailbox -> IO ()
receiver conn mailbox  = receiveMessages
  where
    receiveMessages = catch (do
      infoM _log $ "Waiting to receive from " ++ (show $ connAddress conn)
      socket <- atomically $ readTMVar $ connSocket conn
      maybeMsg <- receiveMessage socket
      infoM _log $ "Received message from " ++ (show $ connAddress conn)
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
      warningM _log $ "No length received"
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
