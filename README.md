courier
=======

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

Both in-delivery-order reception and selective out of order reception are supported: applications designers
are free to choose which style makes sense for a specific application.  The use of the latter technique
approximates an Erlang-style coding model, as messages selected witha supplied function are received before
other non-selected messages that are ahead of them in the queue.

A sample of use follows:

```haskell
 -- Just import this package to access the primary APIs
 import Network.Endpoints

 -- A specific transport is necessary, however
 import Network.Transport.TCP
 
 helloWorld :: IO ()
 helloWorld = do
   let name1 = "endpoint1"
       name2 = "endpoint2"
       resolver = resolverFromList [(name1,"localhost:2000"),
                                    (name2,"localhost:2001")]
   transport <- newTCPTransport resolver
   endpoint1 <- newEndpoint [transport]
   endpoint2 <- newEndpoint [transport]
   Right () <- bindEndpoint endpoint1 name1
   Right () <- bindEndpoint endpoint2 name2
   sendMessage endpoint1 name2 $ encode "hello world!"
   msg <- receiveMessage endpoint2
   print msg
   shutdown transport
   
```

To install, simply run the following in a shell:

```
cabal install courier
```

Or if using cabal-dev, run in a directory prepared with a cabal-dev sandbox:

```
cabal-dev install courier
```

Successfully installed on both Linux 12.04 AMD64 and Mac OS X Mount Lion, using GHC 7.6.3.
