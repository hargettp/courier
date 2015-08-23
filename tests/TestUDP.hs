module TestUDP (
  tests4
  ,tests6
) where

-- local imports

import Network.Transport.Sockets.UDP

import TransportTestSuite

-- external imports

import Test.Framework

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

tests4 :: IO [Test.Framework.Test]
tests4 = do
  name1 <- newUDPAddress
  name2 <- newUDPAddress
  name3 <- newUDPAddress
  name4 <- newUDPAddress
  return $ transportTestSuite
      (newUDPTransport4 udpSocketResolver4)
      "udp4"
      name1
      name2
      name3
      name4

tests6 :: IO [Test.Framework.Test]
tests6 = do
  name1 <- newUDPAddress6
  name2 <- newUDPAddress6
  name3 <- newUDPAddress6
  name4 <- newUDPAddress6
  return $ transportTestSuite
    (newUDPTransport6 udpSocketResolver6)
    "udp6"
    name1
    name2
    name3
    name4

_log :: String
_log = "_test_UDP"

-- UDP tests
