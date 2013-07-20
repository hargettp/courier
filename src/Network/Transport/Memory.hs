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
import qualified Data.ByteString as B
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
  addressIdentifier = address
  }

data MemoryTransport = MemoryTransport {
  boundMailboxes :: TVar (M.Map Address Mailbox)
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
      handles = memoryHandles,
      bind = memoryBind transport,
      sendTo = memorySendTo transport,
      shutdown = return ()
      }

memoryBind :: MemoryTransport -> Mailbox -> Address -> IO (Either String Binding)
memoryBind transport mailbox address = do
  atomically $ modifyTVar (boundMailboxes transport) 
    (\mailboxes -> M.insert address mailbox mailboxes)
  return $ Right Binding {
    bindingAddress = address,
    unbind = memoryUnbind transport address
    }
                                       
memoryHandles :: Address -> Bool
memoryHandles address = memoryScheme == (addressScheme address)

memorySendTo :: MemoryTransport -> Address -> B.ByteString -> IO ()
memorySendTo transport address msg = do
  mailboxes <- atomically $ readTVar $ boundMailboxes transport
  let mailbox = fromJust $ M.lookup address mailboxes
  atomically $ writeTQueue mailbox msg

memoryUnbind :: MemoryTransport -> Address -> IO ()
memoryUnbind transport address = do
  atomically $ modifyTVar (boundMailboxes transport) deleteBinding
  where deleteBinding m = M.delete address m