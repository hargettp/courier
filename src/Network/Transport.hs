{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Transport
-- Copyright   :  (c) Phil Hargett 2013
-- License     :  MIT (see LICENSE file)
-- 
-- Maintainer  :  phil@haphazardhouse.net
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- A 'Transport' abstracts the details of message delivery, and defines the interfaces
-- that specific 'Transport' implementations should provide in order to deliver messages
-- for 'Endpoint's.
--
-----------------------------------------------------------------------------

module Network.Transport (
  Address(..),
  Binding(..),
  Mailbox,
  newMailbox,
  Scheme,
  Transport(..),  
  ) where

-- local imports

-- external imports
import Control.Concurrent.STM
import Data.ByteString as B
import GHC.Generics
import Data.Serialize

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-|
A 'Mailbox' is a place where transports can put messages for 'Network.Endpoint.Endpoint's
to receive.  Typically 'Network.Endpoint.Endpoint's will use the same 'Mailbox' when
binding or connecting with a 'Transport'.
-}
type Mailbox = TQueue B.ByteString

{-|
Create a new mailbox.
-}
newMailbox :: IO Mailbox
newMailbox = atomically $ newTQueue

{-|
An address is a logical identifier suitable for establishing a 'Connection' to
another 'Endpoint' over a 'Transport'. The 'addressScheme' determines which 'Transport'
will handle attempts to 'connect' or 'bind' to an 'Address'.
-}
data Address = Address {
  addressScheme :: String,
  addressIdentifier :: String
  } deriving (Eq,Show,Ord,Generic)

instance Serialize Address

{-|
A scheme is an identifier for a discrete type of transport.
-}
type Scheme = String

{-|
Bindings are a site for receiving messages on a particular 'Address'
through a 'Transport'.
-}
data Binding = Binding {
  bindingAddress :: Address,
  unbind :: IO ()
  }

{-|
A 'Transport' defines a specific method for establishing connections
between 'Endpoint's.
-}
data Transport = Transport {
  scheme :: String,
  handles :: Address -> Bool,
  bind :: Mailbox -> Address -> IO (Either String Binding),
  sendTo :: Address -> B.ByteString -> IO (),
  shutdown :: IO ()
  }
