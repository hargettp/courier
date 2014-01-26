0.1.0.9

    * Added HelloWorld example, and updated code sample in documentation to reflect
      an example workking with the current API
      
    * Early implementation of synchronous RPCs on top of endpoints, with basic unit tests.
      
0.1.0.8

 * Enabled selective out of order mesage reception, by using a Mailbox, an extension of STM's
   TQueues that extracts the next message from the queue that matches a supplied selection
   function regardless of message order in the queue.

   With selective out of order message reception, endpoints can approximate the
   Erlang-style of message delivery and composing applications that must handle multiple
   message types becomes simpler. If an application needs to handle multiple message types,
   it can choose to run separate separate message pumps concurrently, each handling different
   types of messages. Each message pump can operate independently of one another.

 * Fixed issue #2 and other interim bugs all resulting from multiple name bindings that resolve
   the same underlying address.  Now, bound sockets are reused (with reference counting) so 
   that if there are multiple bindings to the same address only 1 socket is created and user.

 * Unit tests passing on Mac OS X again, mostly due to correct management of sockets and 
   appropriate reuse.
 
0.1.0.7

  * First inclusion of changelog in package
  
0.1.0.6

 * Added simple implementation of transport for UDP
 * Verified build & unit tests on Linux Ubuntu 13.10 with GHC 7.6.3
 * Verified build & unit tests on pre-release GHC 7.8 on FP Complete's platform
 * Mac OS X 10.9 is unreliable for unit tests, do to non-deterministic socket bind failures
