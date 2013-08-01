{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Transport.TCP
-- Copyright   :  (c) Phil Hargett 2013
-- License     :  MIT (see LICENSE file)
-- 
-- Maintainer  :  phil@haphazardhouse.net
-- Stability   :  experimental
-- Portability :  non-portable (uses STM)
--
-- TCP transports deliver messages to other 'Network.Endpoints.Endpoint's using TCP/IP.
--
-- Each TCP transport manages both socket bindings and connections on behalf of
-- 'Endpoint's, dynamically opening / closing new sockets as needed to deliver
-- messages to other 'Endpoint's using TCP transports.
-----------------------------------------------------------------------------

module Network.Transport.TCP (
  newTCPTransport,
  newTCPAddress
  ) where

-- local imports

import Network.Transport

-- external imports

import Control.Concurrent.Async
import Control.Concurrent.STM

import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Serialize

import GHC.Generics

import Network.Socket (HostName,ServiceName,Socket,sClose,accept)
import Network.Simple.TCP hiding (accept)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data TCPTransport = TCPTransport {
  tcpListeners :: TVar (M.Map ServiceName Socket),
  tcpMessengers :: TVar (M.Map Address Messenger)
  }
                    
data IdentifyMessage = IdentifyMessage Address deriving (Generic)

instance Serialize IdentifyMessage

{-|
Create an 'Address' suitable for use with TCP 'Transport's
-}
newTCPAddress :: String -> Address
newTCPAddress address = Address {
  addressScheme = tcpScheme,
  addressIdentifier = address
  }
                        
{-|
Create a new 'Transport' suitable for sending messages over TCP/IP.  There can
be multiple instances of these 'Transport's: 'Network.Endpoints.Endpoint' using
different instances will still be able to communicate, provided they use
correct TCP/IP addresses (or hostnames) for communication.
-}
newTCPTransport :: IO Transport
newTCPTransport = do 
  listeners <- atomically $ newTVar M.empty
  messengers <- atomically $ newTVar M.empty
  let transport = TCPTransport {
        tcpListeners = listeners,
        tcpMessengers = messengers
        }
  return Transport {
      scheme = tcpScheme,
      handles = tcpHandles,
      bind = tcpBind transport,
      sendTo = tcpSendTo transport,
      shutdown = tcpShutdown transport
      }

--------------------------------------------------------------------------------
                        
{-|
Parse a TCP 'Address' into its respective 'HostName' and 'PortNumber' components, on the
assumption the 'Address' has an identifer in the format @host:port@. If
the port number is missing from the supplied address, it will default to 0.  If the
hostname component is missing from the identifier (e.g., just @:port@), then hostname
is assumed to be @localhost@.
-}
parseTCPAddress :: Address -> (HostName,ServiceName)
parseTCPAddress address = 
  if tcpHandles address then
    let identifer = T.pack $ addressIdentifier address 
        parts = T.splitOn ":" identifer
    in if (length parts) > 1 then
         (host $ T.unpack $ parts !! 0, port $ T.unpack $ parts !! 1)
       else (host $ T.unpack $ parts !! 0, "0")
    else error $ "Not a TCP addres: " ++ (show address)
  where
    host h = if h == "" then
               "localhost"
             else h
    port p = p

tcpScheme :: Scheme
tcpScheme = "tcp"

tcpHandles :: Address -> Bool
tcpHandles address = (addressScheme address) == tcpScheme

tcpBind :: TCPTransport -> Mailbox -> Address -> IO (Either String Binding)
tcpBind transport inc address = do  
  let (_,port) = parseTCPAddress address
  listener <- async $ tcpListen port
  return $ Right Binding {
    bindingAddress = address,
    unbind = tcpUnbind listener
    }
  where
    tcpListen port = listen HostAny port $ \(socket,_) -> tcpAccept socket
    tcpAccept socket = do
      (client,_) <- accept socket
      _ <- async $ tcpDispatch client
      tcpAccept socket
    tcpDispatch client = do
      identity <- tcpIdentify client
      case identity of
        Nothing -> sClose client
        Just (IdentifyMessage clientAddress) -> do
          msngr <- newMessenger client clientAddress inc
          found <- atomically $ do 
            msngrs <- readTVar $ tcpMessengers transport
            return $ M.lookup clientAddress msngrs
          case found of
            Just _ -> closeMessenger msngr
            Nothing -> do
              atomically $ do 
                modifyTVar (tcpMessengers transport) $ (\msngrs -> M.insert address msngr msngrs)
    tcpIdentify client = do
      maybeMsg <- receiveMessage client
      case maybeMsg of
        Nothing -> return Nothing
        Just bytes -> do
          let msg = decode bytes
          case msg of
            Left _ -> return Nothing
            Right message -> return $ Just message
    tcpUnbind listener = cancel listener

tcpSendTo :: TCPTransport -> Address -> Message -> IO ()
tcpSendTo transport address msg = do
  amsngr <- atomically $ do 
    msngrs <- readTVar $ tcpMessengers transport
    return $ M.lookup address msngrs
  case amsngr of
    Nothing -> return ()
    Just msngr -> atomically $ writeTQueue (messengerOut msngr) msg 

tcpShutdown :: TCPTransport -> IO ()
tcpShutdown _ = return ()

data Messenger = Messenger {
  messengerOut :: Mailbox,
  messengerAddress :: Address,
  messengerSender :: Async (),
  messengerReceiver :: Async (),
  messengerSocket :: Socket
  }
                 
newMessenger :: Socket -> Address -> Mailbox -> IO Messenger                 
newMessenger socket address inc = do
  out <- newMailbox
  sndr <- async $ sender socket out
  rcvr <- async $ receiver socket inc
  return Messenger {
    messengerOut = out,
    messengerAddress = address,
    messengerSender = sndr,
    messengerReceiver = rcvr,
    messengerSocket = socket
    }
                 
closeMessenger :: Messenger -> IO ()                 
closeMessenger msngr = do
  cancel $ messengerSender msngr
  cancel $ messengerReceiver msngr
  sClose $ messengerSocket msngr

sender :: Socket -> Mailbox -> IO ()
sender socket mailbox = sendMessages
  where
    sendMessages = do
      msg <- atomically $ readTQueue mailbox
      send socket $ encode (B.length msg)
      send socket msg
      sendMessages

receiver :: Socket -> Mailbox -> IO ()
receiver socket mailbox  = receiveMessages
  where
    receiveMessages = do
      maybeMsg <- receiveMessage socket
      case maybeMsg of
        Nothing -> return ()
        Just msg -> atomically $ writeTQueue mailbox msg
        
receiveMessage :: Socket -> IO (Maybe Message)    
receiveMessage socket = do
  maybeLen <- recv socket 8 -- TODO must figure out what defines length of an integer in bytes 
  case maybeLen of
    Nothing -> return Nothing
    Just len -> recv socket $ msgLength (decode len)
  where
    msgLength (Right size) = size
    msgLength (Left err) = error err
