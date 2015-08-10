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
  SocketConnections,
  socketConnect,
  SocketBinding(..),
  SocketBindings,

  messenger,
  connector,

  Resolver(..),
  resolve1,
  socketResolver
  ) where

-- local imports

import Network.Endpoints

-- external imports

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception

import qualified Data.Map as M

import qualified Network.Socket as NS

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

type Connect = Endpoint -> Name -> IO SocketConnection
-- type Disconnect = IO ()

data Resolver = Resolver {
  resolve :: Name -> IO [NS.SockAddr]
  }

resolve1 :: Resolver -> Name -> IO NS.SockAddr
resolve1 resolver name = do
  addresses <- resolve resolver name
  return $ head addresses

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

data SocketConnection = SocketConnection {
  disconnectSocketConnection :: IO (),
  sendSocketMessage :: Message -> IO (),
  receiveSocketMessage :: IO Message
}

type SocketConnections = TVar (M.Map Name (Async ()))

data SocketBinding = SocketBinding {
  socketBindingName :: Name,
  socketListener :: TMVar (Async () )
}

type SocketBindings = TVar (M.Map Name SocketBinding)
-- type SocketBindings = TVar (M.Map Name (Async ()))

socketConnect :: SocketConnections -> Connect -> Endpoint -> Name  -> IO Connection
socketConnect vConnections factory endpoint name = do
  alreadyConnected <- atomically $ do
    connections <- readTVar vConnections
    return $ M.lookup name connections
  case alreadyConnected of
    Nothing -> do
      conn <- async $ connector endpoint name factory
      atomically $ modifyTVar vConnections $ M.insert name conn
      return Connection {
        connectionDestination = name,
        disconnect = cancel conn
      }
    Just _ -> throw $ ConnectionExists name

connector :: Endpoint -> Name -> Connect -> IO ()
connector endpoint name transportConnect = loopUntilKilled $ do
  connection <- transportConnect endpoint name
  finally (messenger endpoint name connection) $
    disconnectSocketConnection connection
  where
    loopUntilKilled fn =
      catch (catch fn untilKilled)
        (loop fn)
    loop :: IO () -> SomeException -> IO ()
    loop fn _ = loopUntilKilled fn
    untilKilled :: AsyncException -> IO ()
    untilKilled _ = return ()

messenger :: Endpoint -> Name -> SocketConnection -> IO ()
messenger endpoint name connection =
  -- counting on race_ to kill reader & writer
  -- if messenger is killed; since it uses withAsync, it should
  race_ reader writer
  where
    reader = do
      msg <- receiveSocketMessage connection
      atomically $ postMessage endpoint msg
      reader
    writer = do
      msg <- atomically $ pullMessage endpoint name
      sendSocketMessage connection msg
      writer
