courier
=======

A basic message-passing library, intended for simplifying network applications.

Inspired by Erlang's simple message-passing facilities, courier provides roughly similar capabilities.  Applications simply 
create one or more endpoints, bind each to a transport using a given name, then can freely send / receive messages to
other endpoints just by referencing the name each endpoint bound to its transport.

A primary driver of courier's design is the ability of application writers to swap out the transport used for
delivering messages between endpoints without altering the logic of their application.  Many algorithms (such as for
distributed consensus) require a simplified message-passing facility in their implementation, and keeping the
transport implementation separate from the specific messages passing protocol can simplify development and testing of
such algorithms.

A sample of use follows:

```haskell
 -- Just import this package to access the primary APIs
 import Network.Endpoints

 -- A specific transport is necessary, however
 import Network.Transport.TCP
 
 helloWorld :: IO ()
 helloWorld = do
   let name1 = "endpoint1"b
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
