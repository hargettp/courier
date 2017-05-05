0.1.1.5

  * Added withClient / withServer helpers
  * Removed support for 7.6 / 7.8, as they are before the FAMP proposal was         
    implemented (https://wiki.haskell.org/Functor-Applicative-Monad_Proposal)
  * Added typed equivalents for a selection of functions in Network.RPC

0.1.1.4

  * Corrected an issue with memory transport where a message to an unbound destination would be dropped

0.1.1.3

  * Changed with... functions to return specified types in IO, instead of always returning (); not technically a
    breaking change, but does prevent backward compatibility from here
  * Memory transport flushes to destinations before completing shutdown
  * Linting and first compile against GHC 8.0.1; continuing to support back to 7.6 for now

0.1.1.2

  * Updated README with better working example
  * Removed warning about TCP/IP transport; it's working again

0.1.1.1

There are significant changes in this release, due to a complete rewrite of TCP connection handling, and,
in so doing, a rewrite of the basic model for how transports and endpoints interact. This rewrite fixes
bug #10.

Endpoints now simply have inbound and outbound mailboxes, and a set of names to which the endpoint is bound
via a transport. The intent is that endpoints are discrete entities orthogonal to the transport which moves
messages between them. Its the job of the transport to move messages from the outbound mailbox of one endpoint
to the inbound mailbox of the endpoint bound to the name specified in the envelope for a given message.

New helpers for managing the lifecycle of bindings and connections now exist, and also a helper for setting the name
of a client.

0.1.0.15

    * Initial implementation of IPv6 transports (TCP and UDP)
    * Small cleanup for better thread safety
    * Refactored socket transports for more code sharing
    * Added anyCall
    * Exported RequestId and mkRequestId
    * Incorporated changes from carletes for running tests in parallel by allowing
    each test to bind to their own set of ports; on machines with multiple CPUs the speedup
    has a noticeable impact on producitivity
    * Added methodSelector for applications that may want to customize RPC handling

0.1.0.14

    * Included test source into tarball

0.1.0.13

    * Explicitly removed support for 7.4; 7.6 now required.

0.1.0.12

    * Fixed a long-standing issue concerning inconsistent failures. Turns out, on operating
    systems with both IPv4 and IPv6 support, the TCP and UDP transports (which were really
    only intended to be IPv4 for now) were not properly finding IPv4 addresses--in some
    cases, IPv6 were being used instead, resulting in errors.  Both TCP and UDP now
    pass (IPv4 only, of course), and in the future, IPv6 versions of these transports
    will be added.

0.1.0.11

    * API breaking change: RPC functions no longer take / return instances of Serialize,
    but they take / return Message values instead.  The reason for this change is that
    while courier does use Serialize internally to simplify a variety of operations,
    putting instances of Serialize in RPC functions was misleading: applications would
    still need to take care to ensure that different RPC requests / responses could
    be differentiated, otherwise courier might accidentally deserialize a message
    to the wrong type at an application's request (e.g., inside a hear callback). This
    can lead to decoding errors, which may actually causes exceptions to be thrown
    in pure methods.  By leaving the interface to use Message values, its not only
    consistent with the rest of Endpoint behavior, but it clarifies the responsibility
    of the calling application to manage their Message payloads on their own.

    * Improvements to RPC. Previously, it was not possible to differentiate correctly
    between requests and responses in the same mailbox: the only check was that
    deserialization succeeded, which wasn't a sufficient test, and it would be possible
    to deserialize a response as a request, etc.  This lead to spurious errors.

    * Added functions for detecting presence of messages in an an endpoint's mailbox
    (while still leaving them unconsumed): mostly added this for applications that
    need to react to the presence of a particular message, but not actual consume
    such messages.

    * Exposed the Request / Response types used by RPC.  Applications may benefit
    from detecting them or otherwise manipulating them outside of normal RPC
    flow, they are exposed here for that purpose.

0.1.0.10

    * Added hear primitive as one-shot counterpart to call.  While handle
    may be generally useful for many applications as it fully encapsulates repeated
    handling of incoming requests, hear adds the ability to wait for a single
    incoming request. Hear also returns a function of type Reply suitable for sending
    a single response to the caller. Being more low-level in nature, some applications
    find direct use of hear more flexible than the higher-level handle construct.

    * Fixed a mistake in 0.1.0.9 where RPCs where supposed to have unique identifiers
    for matching up a call with a response.  The value was never computed, when it
    should have been a random value for every call, so that responses could be matched
    with their originating request.  Now using Data.UUID.V4 to generate unique identifiers
    for each request; dependency added in cabal.

    * Unit tests on OS X 10.9 unreliable again, with both ghc 7.6.3 and ghc 7.8.1 (rc1)

    * Unit tests passing on Ubuntu 12.04 LTS AMD64 ghc 7.6.3

    * Unit tests passing on FP Complete with 7.8 preview compiler

0.1.0.9

    * Added HelloWorld example, and updated code sample in documentation to reflect
      working with the current API

    * Early implementation of synchronous RPCs on top of endpoints, with basic unit tests.
      Both single and group RPCs are implemented, as are calls with definite and indefinite
      waits for responses.

    * Removed dependency on network-simple

    * Improved documentation throughout.

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
