-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Transport.Memory
-- Copyright   :  (c) Phil Hargett 2013
-- License     :  MIT (see LICENSE file)
--
-- Maintainer  :  phil@haphazardhouse.net
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- Memory transports deliver messages to other 'Network.Endpoints.Endpoint's within the same shared
-- address space, or operating system process.

-- Internally memory transports use a set of 'TQueue's to deliver messages to 'Network.Endpoint.Endpoint's.
-- Memory transports are not global in nature: 'Network.Endpoint.Endpoint's can only communicate with
-- one another if each has added the same memory 'Transport' and each invoked 'bind' on that shared
-- transport.
--
-----------------------------------------------------------------------------

module Network.Transport.Memory (
  newMemoryTransport
  ) where

-- local imports

import Network.Endpoints

-- external imports

import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Data.Map as M

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-|
Create a new memory 'Transport' for use by 'Network.Endpoint.Endpoint's.
-}
-- data Transport = Transport {
--   bind :: Endpoint -> Name -> IO (Either String Binding),
--   connect :: Endpoint -> Name -> IO (Either String Connection),
--   shutdown :: IO ()
--   }
newMemoryTransport :: IO Transport
newMemoryTransport = do
  vBindings <- atomically $ newTVar M.empty
  vConnections <- atomically $ newTVar M.empty
  return Transport {
      bind = memoryBind vBindings,
      connect = memoryConnect vConnections vBindings,
      shutdown = return ()
      }

memoryBind :: Bindings -> Endpoint -> Name -> IO (Either String Binding)
memoryBind vBindings endpoint name = atomically $ do
  bindings <- readTVar vBindings
  case M.lookup name bindings of
    Nothing -> do
      modifyTVar vBindings $ M.insert name endpoint
      return $ Right Binding {
        bindingName = name,
        unbind = memoryUnbind vBindings name
      }
    Just _ -> return $ Left "binding exists"

memoryUnbind :: Bindings -> Name -> IO ()
memoryUnbind vBindings name = atomically $ modifyTVar vBindings $ M.delete name

type Bindings = TVar (M.Map Name Endpoint)

memoryConnect :: Connections -> Bindings -> Endpoint -> Name -> IO (Either String Connection)
memoryConnect vConnections vBindings endpoint name = do
  transmitter <- async $ memoryTransmitter endpoint name vBindings
  atomically $ do
    connections <- readTVar vConnections
    case M.lookup name connections of
      Nothing -> do
        modifyTVar vConnections $ M.insert name endpoint
        return $ Right Connection {
          connectionDestination = name,
          disconnect = memoryDisconnect vConnections name transmitter
        }
      Just _ -> return $ Left "connection exists"

type Connections = TVar (M.Map Name Endpoint)

memoryDisconnect :: Connections -> Name -> Async () -> IO ()
memoryDisconnect vConnections name transmitter = do
  cancel transmitter
  atomically $ modifyTVar vConnections $ M.delete name

memoryTransmitter :: Endpoint -> Name -> Bindings -> IO ()
memoryTransmitter origin peer vBindings = do
  atomically $ do
    bindings <- readTVar vBindings
    case M.lookup peer bindings of
      Nothing -> retry
      Just destination -> do
        msg <- pullMessage origin peer
        postMessage destination msg
  memoryTransmitter origin peer vBindings
