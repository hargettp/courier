-----------------------------------------------------------------------------
-- |
-- Module      :  HelloWorld
-- Copyright   :  (c) Phil Hargett 2015
-- License     :  MIT (see LICENSE file)
--
-- Maintainer  :  phil@haphazardhouse.net
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- Basic code sample for explaining how to use courier.
--
-----------------------------------------------------------------------------

module Main where

 -- Import this package to manage endpoints
import Network.Endpoints

 -- A specific transport is necessary, however
import Network.Transport.Sockets.TCP

-- Needed for serialization
import Data.Serialize

main :: IO ()
main = do
  -- each endpoint needs a name; since we're using TCP
  -- as our transport, they need to be host/port pairs
  let name1 = Name "localhost:9001"
      name2 = Name "localhost:9002"
      -- the default resolvers just pull apart a name into separate
      -- host and port components; more elaborate resolvers could
      -- perform name lookups or other translations
      resolver = tcpSocketResolver4
  -- we need endpoints for each end of the communication
  endpoint1 <- newEndpoint
  endpoint2 <- newEndpoint
  -- we need a transport to move messages between endpoints
  withTransport (newTCPTransport4 resolver) $ \transport ->
    withEndpoint transport endpoint1 $
      withEndpoint transport endpoint2 $
      -- the first endpoint is just a client, so it needs a name to receive
      -- responses, but does not need a binding since it isn't accept connections
        withName endpoint1 name1 $
          -- the second endpoint is a server, so it needs a binding
          withBinding transport endpoint2 name2 $
            -- a connection between the first endpoint and the name of the second
            -- creates a bi-directional path for messages to flow between the endpoints
            withConnection transport endpoint1 name2 $ do
              sendMessage endpoint1 name2 $ encode "hello world!"
              msg <- receiveMessage endpoint2
              let Right txt = decode msg
                  in print (txt :: String)
