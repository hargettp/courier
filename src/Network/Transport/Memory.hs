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
  newMemoryAddress
  ) where

-- local imports

import Network.Transport

-- external imports

import Control.Concurrent.STM
import qualified Data.Map as M
import Data.Maybe (fromJust)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

memoryScheme :: Scheme
memoryScheme = "mem"

{-|
Create an 'Address' suitable for use with memory 'Transport's
-}
newMemoryAddress :: String -> Address
newMemoryAddress address = Address {
  addressScheme = memoryScheme,
  addressLocation = address
  }

data MemoryTransport = MemoryTransport {
  boundMailboxes :: TVar (M.Map Name Mailbox)
  }
                       
{-|
Create a new memory 'Transport' for use by 'Network.Endpoint.Endpoint's.
-}
newMemoryTransport :: IO Transport
newMemoryTransport = do 
  bindings <- atomically $ newTVar M.empty
  let transport = MemoryTransport {
        boundMailboxes = bindings
        }
  return Transport {
      scheme = memoryScheme,
      handles = memoryHandles transport,
      bind = memoryBind transport,
      sendTo = memorySendTo transport,
      shutdown = return ()
      }

memoryBind :: MemoryTransport -> Mailbox -> Name -> IO (Either String Binding)
memoryBind transport mailbox name = do
  atomically $ modifyTVar (boundMailboxes transport) 
    (\mailboxes -> M.insert name mailbox mailboxes)
  return $ Right Binding {
    bindingName = name,
    unbind = memoryUnbind transport name
    }
                                       
memoryHandles :: MemoryTransport -> Name -> IO Bool
-- memoryHandles transport name = True
memoryHandles _ _ = return True

memorySendTo :: MemoryTransport -> Name -> Message -> IO ()
memorySendTo transport name msg = do
  mailboxes <- atomically $ readTVar $ boundMailboxes transport
  let mailbox = fromJust $ M.lookup name mailboxes
  atomically $ writeTQueue mailbox msg

memoryUnbind :: MemoryTransport -> Name -> IO ()
memoryUnbind transport name = do
  atomically $ modifyTVar (boundMailboxes transport) deleteBinding
  where deleteBinding m = M.delete name m