{-# LANGUAGE DeriveDataTypeable     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Transport
-- Copyright   :  (c) Phil Hargett 2015
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

  -- * Transport
  Transport(..),
  TransportException(..),

  withEndpoint,
  withEndpoint2,
  withEndpoint3,
  withEndpoint4,

  Binding(..),
  BindException(..),
  withBinding,
  withBinding2,
  withBinding3,
  withBinding4,

  Connection(..),
  ConnectException(..),
  withConnection,
  withConnection2,
  withConnection3,
  withConnection4,

) where

-- local imports

import Network.Endpoints

-- external imports

-- import Control.Concurrent.Mailbox
import Control.Concurrent.STM
import Control.Exception

import qualified Data.Set as S
import Data.Typeable

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-|
A 'Transport' defines a specific method for establishing connections
between 'Endpoint's.
-}
data Transport = Transport {
  bind :: Endpoint -> Name -> IO Binding,
  connect :: Endpoint -> Name -> IO Connection,
  shutdown :: IO ()
  }

data TransportException =
  NoDataRead |
  DataUnderflow
  deriving (Show,Typeable)

instance Exception TransportException

withEndpoint :: Transport -> (Endpoint -> IO ()) -> IO ()
withEndpoint transport communicator =
  finally
    (newEndpoint >>= communicator)
    (shutdown transport)

withEndpoint2 :: Transport -> (Endpoint -> Endpoint -> IO ()) -> IO ()
withEndpoint2 transport fn =
  withEndpoint transport $ \endpoint1 ->
    withEndpoint transport $ \endpoint2 -> fn endpoint1 endpoint2

withEndpoint3 :: Transport -> (Endpoint -> Endpoint -> Endpoint -> IO () ) -> IO ()
withEndpoint3 transport fn =
  withEndpoint transport $ \endpoint1 ->
    withEndpoint transport $ \endpoint2 ->
      withEndpoint transport $ \endpoint3 -> fn endpoint1 endpoint2 endpoint3

withEndpoint4 :: Transport -> (Endpoint -> Endpoint -> Endpoint -> Endpoint -> IO () ) -> IO ()
withEndpoint4 transport fn =
  withEndpoint2 transport $ \endpoint1 endpoint2 ->
    withEndpoint2 transport $ \endpoint3 endpoint4 -> fn endpoint1 endpoint2 endpoint3 endpoint4

{-|
Bindings are a site for receiving messages on a particular 'Name'
through a 'Transport'.
-}
data Binding = Binding {
  bindingName :: Name,
  unbind :: IO ()
  }

data BindException =
  BindingExists Name |
  BindingDoesNotExist Name
  deriving (Show,Typeable)

instance Exception BindException

withBinding :: Transport -> Endpoint -> Name -> IO () -> IO ()
withBinding transport endpoint name actor = do
  atomically $ do
    bindings <- readTVar $ boundEndpointNames endpoint
    if S.member name bindings
      then throw $ BindingExists name
      else modifyTVar (boundEndpointNames endpoint) $ S.insert name
  binding <- bind transport endpoint name
  finally actor $ do
    unbind binding
    atomically $ modifyTVar (boundEndpointNames endpoint) $ S.delete name

withBinding2 :: Transport -> (Endpoint,Name) -> (Endpoint,Name) -> IO () -> IO ()
withBinding2 transport (endpoint1,name1) (endpoint2,name2) fn =
  withBinding transport endpoint1 name1 $
    withBinding transport endpoint2 name2 fn

withBinding3 :: Transport -> (Endpoint,Name) -> (Endpoint,Name) -> (Endpoint,Name) -> IO () -> IO ()
withBinding3 transport (endpoint1,name1) (endpoint2,name2) (endpoint3,name3) fn =
  withBinding transport endpoint1 name1 $
    withBinding transport endpoint2 name2 $
      withBinding transport endpoint3 name3 fn

withBinding4 :: Transport -> (Endpoint,Name) -> (Endpoint,Name) -> (Endpoint,Name) -> (Endpoint,Name) -> IO () -> IO ()
withBinding4 transport (endpoint1,name1) (endpoint2,name2) (endpoint3,name3) (endpoint4,name4) fn =
  withBinding transport endpoint1 name1 $
    withBinding transport endpoint2 name2 $
      withBinding transport endpoint3 name3 $
        withBinding transport endpoint4 name4 fn

{-
Connections are pathways for sending messages to an 'Endpoint' bound to a specific 'Name'
-}
data Connection = Connection {
  disconnect :: IO ()
}

data ConnectException =
  ConnectionExists Name |
  ConnectionDoesNotExist Name |
  ConnectionHasNoBoundPeer Name
  deriving (Show,Typeable)

instance Exception ConnectException

withConnection :: Transport -> Endpoint -> Name -> IO () -> IO ()
withConnection transport endpoint name actor = do
  connection <- connect transport endpoint name
  finally actor $ disconnect connection

withConnection2 :: Transport -> Endpoint -> Name -> Name -> IO () -> IO ()
withConnection2 transport endpoint name1 name2 communicator =
  withConnection transport endpoint name1 $
    withConnection transport endpoint name2 communicator

withConnection3 :: Transport -> Endpoint -> Name -> Name -> Name -> IO () -> IO ()
withConnection3 transport endpoint name1 name2 name3 communicator =
  withConnection transport endpoint name1 $
    withConnection transport endpoint name2 $
      withConnection transport endpoint name3 communicator

withConnection4 :: Transport -> Endpoint -> Name -> Name -> Name -> Name -> IO () -> IO ()
withConnection4 transport endpoint name1 name2 name3 name4 communicator =
  withConnection transport endpoint name1 $
    withConnection transport endpoint name2 $
      withConnection transport endpoint name3 $
        withConnection transport endpoint name4 communicator
