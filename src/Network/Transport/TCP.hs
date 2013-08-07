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
import Control.Exception

import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Serialize

import GHC.Generics

import Network.Socket (HostName,ServiceName,Socket,sClose,accept)
import Network.Simple.TCP hiding (accept)

import System.Log.Logger

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

_log :: String
_log = "transport.tcp"

data TCPTransport = TCPTransport {
  tcpListeners :: TVar (M.Map ServiceName Socket),
  tcpMessengers :: TVar (M.Map Address Messenger),
  tcpBindings :: TVar (M.Map Address Mailbox),
  tcpInbound :: Mailbox,
  tcpDispatcher :: Async ()
  }
                    
data IdentifyMessage = IdentifyMessage Address deriving (Generic)

instance Serialize IdentifyMessage

{-|
Create an 'Address' suitable for use with TCP 'Transport's
-}
newTCPAddress :: String -> Address
newTCPAddress address = Address {
  addressScheme = tcpScheme,
  addressLocation = address
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
  bindings <- atomically $ newTVar M.empty
  inbound <- newMailbox
  dispatch <- async $ dispatcher bindings inbound
  let transport = TCPTransport {
        tcpListeners = listeners,
        tcpMessengers = messengers,
        tcpBindings = bindings,
        tcpInbound = inbound,
        tcpDispatcher = dispatch
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
    let identifer = T.pack $ addressLocation address 
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
  atomically $ modifyTVar (tcpBindings transport) $ \bindings ->
    M.insert address inc bindings
  let (_,port) = parseTCPAddress address
  listener <- async $ do 
    infoM _log $ "Binding to address " ++ (show address)
    tcpListen port
  return $ Right Binding {
    bindingAddress = address,
    unbind = tcpUnbind listener
    }
  where
    tcpListen port = listen HostAny port $ \(socket,_) -> do 
      tcpAccept socket
    tcpAccept socket = do
      (client,clientAddress) <- accept socket
      _ <- async $ tcpDispatch client clientAddress
      tcpAccept socket
    tcpDispatch client socketAddress = do
      infoM _log $ "Accepted connection on " ++ (show address)
      identity <- tcpIdentify client socketAddress
      case identity of
        Nothing -> sClose client
        Just (IdentifyMessage clientAddress) -> do
          infoM _log $ "Identified " ++ (show clientAddress)
          msngr <- newMessenger client clientAddress (tcpInbound transport)
          found <- atomically $ do 
            msngrs <- readTVar $ tcpMessengers transport
            return $ M.lookup clientAddress msngrs
          case found of
            Just _ -> closeMessenger msngr
            Nothing -> do
              atomically $ do 
                modifyTVar (tcpMessengers transport) $ (\msngrs -> M.insert address msngr msngrs)
    tcpIdentify client clientAddress = do
      infoM _log $ "Awaiting identity from " ++ (show clientAddress)
      maybeMsg <- receiveMessage client
      case maybeMsg of
        Nothing -> return Nothing
        Just bytes -> do
          let msg = decode bytes
          case msg of
            Left _ -> return Nothing
            Right message -> return $ Just message
    tcpUnbind listener = do 
      infoM _log $ "Unbinding from port " ++ (show address)
      cancel listener

tcpSendTo :: TCPTransport -> Address -> Message -> IO ()
tcpSendTo transport address msg = do
  let env = encode $ Envelope {
        envelopeDestination = address,
        envelopeContents = msg
        }
  amsngr <- atomically $ do 
    msngrs <- readTVar $ tcpMessengers transport
    return $ M.lookup address msngrs
  case amsngr of
    Nothing -> do
      let (host,port) = parseTCPAddress address
      infoM _log $ "Connecting to " ++ (show address)
      (socket,_) <- connectSock host port
      infoM _log $ "Connected to " ++ (show address)
      msngr <- newMessenger socket address (tcpInbound transport)
      atomically $ 
        modifyTVar (tcpMessengers transport) $ \msngrs ->
        M.insert address msngr msngrs
      deliver msngr $ encode $ IdentifyMessage address
      deliver msngr env
      return ()
    Just msngr -> deliver msngr env
    where
      deliver msngr message = atomically $ writeTQueue (messengerOut msngr) message

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
  sndr <- async $ sender socket address out
  rcvr <- async $ receiver socket address inc
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

sender :: Socket -> Address -> Mailbox -> IO ()
sender socket address mailbox = sendMessages
  where
    sendMessages = do 
      catch (do
                infoM _log $ "Waiting to send to " ++ (show address)
                msg <- atomically $ readTQueue mailbox
                infoM _log $ "Sending message to " ++ (show address)
                send socket $ encode (B.length msg)
                infoM _log $ "Length sent"
                send socket msg
                infoM _log $ "Message sent to" ++ (show address)
            ) (\e -> do 
                  errorM _log $ "Send error " ++ (show (e :: SomeException))
                  throw e)
      sendMessages

dispatcher :: TVar (M.Map Address Mailbox) -> Mailbox -> IO ()
dispatcher bindings mbox = dispatchMessages
  where
    dispatchMessages = do
      infoM _log $ "Dispatching messages"
      env <- atomically $ readTQueue mbox
      dispatchMessage env
      dispatchMessages
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

receiver :: Socket -> Address -> Mailbox -> IO ()
receiver socket address mailbox  = receiveMessages
  where
    receiveMessages = do
      infoM _log $ "Waiting to receive from " ++ (show address)
      maybeMsg <- receiveMessage socket
      infoM _log $ "Received message from " ++ (show address)
      case maybeMsg of
        Nothing -> return ()
        Just msg -> atomically $ writeTQueue mailbox msg
        
receiveMessage :: Socket -> IO (Maybe Message)    
receiveMessage socket = do
  maybeLen <- recv socket 8 -- TODO must figure out what defines length of an integer in bytes 
  case maybeLen of
    Nothing -> do
      errorM _log $ "No length received"
      return Nothing
    Just len -> do 
      maybeMsg <- recv socket $ msgLength (decode len)
      infoM _log $ "Received message"
      return maybeMsg
  where
    msgLength (Right size) = size
    msgLength (Left err) = error err
