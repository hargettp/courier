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

import Control.Concurrent.STM
import Control.Exception
import Control.Monad

import qualified Data.Map as M
import qualified Data.Set as S

import Text.Printf

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-|
Create a new memory 'Transport' for use by 'Network.Endpoint.Endpoint's.
-}
newMemoryTransport :: IO Transport
newMemoryTransport = do
  vBindings <- atomically $ newTVar M.empty
  vConnections <- atomically $ newTVar M.empty
  return Transport {
      bind = memoryBind vBindings,
      connect = memoryConnect vConnections vBindings,
      shutdown = return ()
      }

memoryBind :: Bindings -> Endpoint -> Name -> IO Binding
memoryBind vBindings endpoint name = atomically $ do
  bindings <- readTVar vBindings
  case M.lookup name bindings of
    Nothing -> do
      modifyTVar (endpointNames endpoint) $ S.insert name
      modifyTVar vBindings $ M.insert name endpoint
      return Binding {
        bindingName = name,
        unbind = memoryUnbind vBindings endpoint name
      }
    Just _ -> throw $ BindingExists name

memoryUnbind :: Bindings -> Endpoint -> Name -> IO ()
memoryUnbind vBindings endpoint name = atomically $ do
  modifyTVar (endpointNames endpoint) $ S.delete name
  modifyTVar vBindings $ M.delete name

type Bindings = TVar (M.Map Name Endpoint)

memoryConnect :: Connections -> Bindings -> Endpoint -> Name -> IO Connection
memoryConnect vConnections vBindings origin name = do
  atomically $ do
    connections <- readTVar vConnections
    case M.lookup name connections of
      Nothing -> do
        bindings <- readTVar vBindings
        case M.lookup name bindings of
          Nothing -> throw $ ConnectionHasNoBoundPeer name
          Just destination -> do
            modifyTVar (endpointOutbound origin) $ M.insert name $ endpointInbound destination
            names <- readTVar $ endpointNames origin
            forM_ (S.elems names) $ \peer -> modifyTVar (endpointOutbound destination) $ M.insert peer $ endpointInbound origin
            modifyTVar vConnections $ M.insert name origin
            return Connection {
              connectionDestination = name,
              disconnect = memoryDisconnect vConnections origin destination name
            }
      Just _ -> throw $ ConnectionExists name

type Connections = TVar (M.Map Name Endpoint)

memoryDisconnect :: Connections -> Endpoint -> Endpoint -> Name -> IO ()
memoryDisconnect vConnections origin destination name = do
  atomically $ do
    modifyTVar (endpointOutbound origin) $ M.delete name
    names <- readTVar $ endpointNames origin
    forM_ (S.elems names) $ \peer -> modifyTVar (endpointOutbound destination) $ M.delete peer
    modifyTVar vConnections $ M.delete name
