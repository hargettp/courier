courier
=======

[![Build Status](https://travis-ci.org/hargettp/courier.svg?branch=master)](https://travis-ci.org/hargettp/courier)

NOTE: As of 4/5/2018, `courier` no longer passes unit tests reliably when building with `cabal`. Please use [stack](https://docs.haskellstack.org/en/stable/README/), as tests continue to pass. 

----

A basic message-passing library, intended for simplifying network applications.

Inspired by Erlang's simple message-passing facilities, courier provides roughly similar capabilities.  Applications simply
create one or more endpoints, bind each to a transport using a given name, then can freely send / receive messages to
other endpoints just by referencing the name each endpoint bound to its transport.

A primary driver of courier's design is the ability of application writers to swap out the transport used for
delivering messages between endpoints without altering the logic of their application.  Many algorithms (such as for
distributed consensus) require a simplified message-passing facility in their implementation, and keeping the
transport implementation separate from the specific message passing protocol can simplify development and testing of
such algorithms.

The semantics of courier's use are simple:

 * An application sends and receives messages through an endpoint
 * Messages are just arbitrary bytestrings; application are free to construct / interpret them as needed
 * Endpoints are created with one or more transports
 * Sending messages is non-blocking and provides no feedback or guarantee regarding delivery; specific transports may,
   however, provide out of band feedback regarding delivery
 * Receiving messages is by default blocking, although a blocking call with a timeout is available

Both in-delivery-order reception and selective out-of-order reception are supported: applications designers
are free to choose which style makes sense for a specific application.  The use of the latter technique
approximates an Erlang-style coding model, as messages selected witha supplied function are received before
other non-selected messages that are ahead of them in the queue.

A sample of use follows:

```haskell
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
```

To install, simply run the following in a shell:

```
cabal install courier
```

Or if using cabal-dev, run in a directory prepared with a cabal-dev sandbox:

```
cabal-dev install courier
```

With the  advent of sandboxes in Cabal 1.18, cabal-dev is no longer necessary,
just use an updated cabal for simplicity. The example assumes you are building
courier from source:

```
cabal clone https://github.com/hargettp/courier.git
cd courier
cabal sandbox init
cabal install --only-dependencies
cabal install
```

Successfully installed on both Linux 12.04 AMD64 and Mac OS X Mount Lion , using GHC 7.6.3.

TODO

 * Ensure that new transports can be defined outside of the courier package and used with courier endpoints
 * Consider adding support for RAW sockets
