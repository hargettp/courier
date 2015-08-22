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
  newMemoryTransport,

  module Network.Transport

) where

-- local imports

import Control.Concurrent.Mailbox
import Network.Endpoints
import Network.Transport

-- external imports

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception

import qualified Data.Map as M

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-|
Create a new memory 'Transport' for use by 'Network.Endpoint.Endpoint's.
-}
newMemoryTransport :: IO Transport
newMemoryTransport = do
  vBindings <- atomically $ newTVar M.empty
  return Transport {
      bind = memoryBind vBindings,
      dispatch = memoryDispatcher vBindings,
      connect = memoryConnect,
      shutdown = return ()
      }

memoryDispatcher :: Bindings -> Endpoint -> IO Dispatcher
memoryDispatcher vBindings endpoint = do
  d <- async disp
  return Dispatcher {
    stop = cancel d
  }
  where
    disp = do
      atomically $ do
        bindings <- readTVar vBindings
        env <- readMailbox $ endpointOutbound endpoint
        case M.lookup (messageDestination env) bindings  of
          Nothing -> return ()
          Just destination -> postMessage destination (envelopeMessage env)
      disp

memoryBind :: Bindings -> Endpoint -> Name -> IO Binding
memoryBind vBindings endpoint name = atomically $ do
  bindings <- readTVar vBindings
  case M.lookup name bindings of
    Nothing -> do
      modifyTVar vBindings $ M.insert name endpoint
      return Binding {
        bindingName = name,
        unbind = memoryUnbind vBindings endpoint name
      }
    Just _ -> throw $ BindingExists name

memoryUnbind :: Bindings -> Endpoint -> Name -> IO ()
memoryUnbind vBindings _ name = atomically $ do
  modifyTVar vBindings $ M.delete name

type Bindings = TVar (M.Map Name Endpoint)

memoryConnect :: Endpoint -> Name -> IO Connection
memoryConnect _ _ =
  return Connection {
    disconnect = return ()
  }
