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
