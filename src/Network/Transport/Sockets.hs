{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Transport.Sockets
-- Copyright   :  (c) Phil Hargett 2015
-- License     :  MIT (see LICENSE file)
--
-- Maintainer  :  phil@haphazardhouse.net
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- Helpers for socket transports.
--
-----------------------------------------------------------------------------
module Network.Transport.Sockets (
  SocketConnection(..),
  Connections,
  socketConnect,

  socketListen,

  messenger,
  connector,

  Resolver,
  ResolverException,
  resolve1,
  socketResolver4,
  socketResolver6,
  wildcard,

  module Network.Transport

) where

-- local imports

import Network.Endpoints
import Network.Transport

-- external imports

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Serialize
import Data.Typeable

import GHC.Generics

import qualified Network.Socket as NS

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

type Connect = Endpoint -> Name -> IO SocketConnection

{-|
A 'Resolver' translates a name into a list of 'NS.SockAddr' for use with a socket-based 'Transport'.
-}
type Resolver = Name -> IO [NS.SockAddr]


{-|
A 'SocketConnection' encapsulates the state of a single outbound connection.
-}
data SocketConnection = SocketConnection {
  connectionDestination :: TMVar Name,
  sendSocketMessage :: Message -> IO (),
  receiveSocketMessage :: IO Message,
  disconnectSocket :: IO ()
}

{-|
Exceptions thrown by a 'Resolver'.
-}
data ResolverException = CannotResolveName Name
  deriving (Show,Typeable)

instance Exception ResolverException

{-|
Socket-based protocols need a way to unambiguously identify who is communicating on a particular 'SocketConnection';
this type accomodates both identifying the sender and sending an actual 'Message'.
-}
data SocketMessage =
  IdentifySender Name
  | SocketMessage Message
  deriving (Generic)

instance Serialize SocketMessage

{-|
Helper to resolve a 'Name' to a single 'NS.SockAddr' for use with a socket-based 'Transport'.
-}
resolve1 :: Resolver -> Name -> IO NS.SockAddr
resolve1 resolve name = do
  addresses <- resolve name
  case addresses of
    [] -> throw $ CannotResolveName name
    (address:_) -> return address

{-|
Simple 'Resolver' for socket-based 'Transport's that just parse's the 'String' inside the 'Name', with the expected
format "host:port"
-}
socketResolver :: NS.Family -> NS.SocketType -> Name -> IO [NS.SockAddr]
socketResolver family socketType name =
  let (host,port) = split nm
      Name nm = name
      hints = NS.defaultHints { NS.addrFlags = [NS.AI_ADDRCONFIG, NS.AI_CANONNAME, NS.AI_NUMERICSERV] }
  in do
      addresses <- NS.getAddrInfo (Just hints) (Just host) (Just port)
      return $ map NS.addrAddress $ filter (\addrInfo -> NS.addrFamily addrInfo == family && NS.addrSocketType addrInfo == socketType) addresses
  where
    split [] = ("","")
    split (w:ws) = case w of
      ':' -> ("",ws)
      _ -> let (w',ws') = split ws
        in (w:w',ws')

{-|
Variation of 'socketResolve' for standard IP addresses.
-}
socketResolver4 :: NS.SocketType -> Name -> IO [NS.SockAddr]
socketResolver4 = socketResolver NS.AF_INET

{-|
Variation of 'socketResolve' for standard IPv6 addresses.
-}
socketResolver6 ::NS.SocketType ->  Name -> IO [NS.SockAddr]
socketResolver6 = socketResolver NS.AF_INET6

{-|
Returns the wildcard 'NS.SockAddr' for the specified 'NS.SockAddr', if possible.
-}
wildcard :: NS.SockAddr -> IO NS.SockAddr
wildcard address =
    case address of
        NS.SockAddrInet port _ -> return $ NS.SockAddrInet port NS.iNADDR_ANY
        NS.SockAddrInet6 port flow _ scope -> return $ NS.SockAddrInet6 port flow NS.iN6ADDR_ANY scope
        _ -> return address

{-|
Listens for new connections on a new 'NS.Socket', and return the 'NS.Socket'.
-}
socketListen :: NS.Family -> NS.SocketType -> Resolver -> Name -> IO NS.Socket
socketListen family socketType resolver name = do
  address <- resolve1 resolver name
  socket <- NS.socket family socketType NS.defaultProtocol
  NS.setSocketOption socket NS.NoDelay 1
  NS.setSocketOption socket NS.ReuseAddr 1
  when (NS.isSupportedSocketOption NS.ReusePort)
    $ NS.setSocketOption socket NS.ReusePort 1
  wildcard address >>= NS.bind socket
  NS.listen socket 2048 -- TODO think about a configurable backlog
  return socket

{-|
Mutble 'M.Map' of 'Name's to 'Connection's.
-}
type Connections = TVar (M.Map Name Connection)

{-|
Establish a 'connector' for exchanging messages from 'Endpoint' with a destination specified by 'Name'.
-}
socketConnect :: Mailboxes -> Connect -> Endpoint -> Name  -> IO Connection
socketConnect mailboxes sConnect endpoint name = do
  connr <- async $ connector mailboxes endpoint name sConnect
  let conn = Connection {
    disconnect = cancel connr
  }
  return conn

{-|
Maintain a 'Connection' between an 'Endpoint' and a destination specified by 'Name'. Should the connection,
the connection will automatically be re-attempted.
-}
connector :: Mailboxes -> Endpoint -> Name -> Connect -> IO ()
connector mailboxes endpoint name transportConnect = loopUntilKilled $ do
  connection <- transportConnect endpoint name
  origins <- atomically $ do
    putTMVar (connectionDestination connection) name
    readTVar $ boundEndpointNames endpoint
  forM_ (S.elems origins) $ \origin ->
    -- atomically $ dispatchMessage mailboxes name $ encode $ IdentifySender origin
    sendSocketMessage connection $ encode $ IdentifySender origin
  finally (messenger mailboxes endpoint connection) $
    disconnectSocket connection
  where
    loopUntilKilled fn =
      catch (catch fn untilKilled)
        (loop fn)
    loop :: IO () -> SomeException -> IO ()
    loop fn _ = loopUntilKilled fn
    untilKilled :: AsyncException -> IO ()
    untilKilled _ = return ()

{-|
Exchange 'Messages' with the 'Endpoint' on the other side of a 'SocketConnection'.
-}
messenger :: Mailboxes -> Endpoint -> SocketConnection -> IO ()
messenger mailboxes endpoint connection =
  -- counting on race_ to kill reader & writer
  -- if messenger is killed; since it uses withAsync, it should
  race_ receiver sender
  where
    receiver = do
      smsg <- receiveSocketMessage connection
      -- TODO consider a way of using a message to identify the name of
      -- the endpoint on the other end of the connection
      case decode smsg of
        Left _ -> return ()
        Right (IdentifySender name) -> atomically $ putTMVar (connectionDestination connection) name
        Right (SocketMessage msg) -> atomically $ postMessage endpoint msg
      receiver
    sender = do
      msg <- atomically $ do
        -- this basically means we wait until we have a name
        name <- readTMVar $ connectionDestination connection
        pullMessage mailboxes name
      sendSocketMessage connection $ encode $ SocketMessage msg
      sender
