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
-- The definition of a transport is deliberately low-level in nature.  Unless a specific
-- transport describes itself as supporting features like guaranteed delivery, applications
-- should NOT assume that message delivery is reliable.
--
-- For example, if a sender sends a message to a name that has not yet been bound, then 
-- immediately waits on the response for that message, then the application may hang, 
-- as the original message may have been dropped.
--
-- However, many application may find it easier to push features such as reliable
-- message delivery into a custom transport, leaving the application only having
-- to concern itself with the messages being delivered rather than how they arrive.
--
-----------------------------------------------------------------------------

module Network.Transport (
  Address,
  Binding(..),
  Envelope(..),
  Message,
  Name,
  Resolver,
  resolve,
  resolverFromList,
  Transport(..),

  module Control.Concurrent.Mailbox
  
  ) where

-- local imports
import Control.Concurrent.Mailbox

-- external imports

import Data.ByteString as B
import Data.Serialize

import GHC.Generics

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-|
Messages are containers for arbitrary data that may be sent to other 'Network.Endpoints.Endpoint's.
-}
type Message = B.ByteString

{-|
Name for uniquely identifying an 'Endpoint'; suitable for identifying
the target destination for a 'Message'.

-}
type Name = String

{-|
An 'Envelope' is a container for a 'Message' with the 'Address' of the 'Message''s destination.

-}
data Envelope = Envelope {
  envelopeDestination :: Name,
  envelopeContents :: Message
  } deriving (Eq,Show,Generic)

instance Serialize Envelope

{-|
A 'Mailbox' is a place where transports can put messages for 'Network.Endpoint.Endpoint's
to receive.  Typically 'Network.Endpoint.Endpoint's will use the same 'Mailbox' when
binding or connecting with a 'Transport'.
-}

{-|
An address is a logical identifier suitable for establishing a connection to
another 'Endpoint' over a 'Transport'. It's use (if at all) is specific to the 'Transport'
in question.
-}
type Address = String

{-|
Bindings are a site for receiving messages on a particular 'Address'
through a 'Transport'.
-}
data Binding = Binding {
  bindingName :: Name,
  unbind :: IO ()
  }

{-|
A 'Transport' defines a specific method for establishing connections
between 'Endpoint's.
-}
data Transport = Transport {
  bind :: Mailbox Message -> Name -> IO (Either String Binding),
  sendTo :: Name -> Message -> IO (),
  shutdown :: IO ()
  }

{-|
A 'Resolver' translates a name into an 'Address', if possible. 
'Transport's may find resolvers useful for determing
where to reach a specific 'Endpoint', given it''s 'Name'.
-}
newtype Resolver = Resolver (Name -> IO (Maybe Address))

{-|
Ask the 'Resolver' to find one or more 'Address'es for the provided
'Name', if any are available from this resolver.

-}
resolve :: Resolver -> Name -> IO (Maybe Address)
resolve (Resolver resolver) name = resolver name

{-|
A simple 'Resolver' that accepts an association list of 'Name's to 'Address'es
and returns the addresses associated with a given name in the list.
-}
resolverFromList :: [(Name,Address)] -> Resolver
resolverFromList addresses = Resolver (\name -> return $ lookup name addresses)
