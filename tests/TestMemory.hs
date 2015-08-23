module TestMemory (tests) where

-- local imports

import Network.Endpoints
import Network.Transport.Memory

import TransportTestSuite

-- external imports

import Test.Framework

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

tests :: [Test.Framework.Test]
tests = transportTestSuite
    newMemoryTransport
    "mem"
    (Name "name1")
    (Name "name2")
    (Name "name3")
    (Name "name4")

_log :: String
_log = "_test_memory"

-- Memory tests
