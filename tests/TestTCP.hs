module TestTCP (
  tests4
  ,tests6
  ) where

-- local imports

import Network.Transport.Sockets.TCP

import TransportTestSuite

-- external imports

import Test.Framework

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

tests4 :: IO [Test.Framework.Test]
tests4 = do
  name1 <- newTCPAddress
  name2 <- newTCPAddress
  name3 <- newTCPAddress
  name4 <- newTCPAddress
  return $ transportTestSuite
      (newTCPTransport4 tcpSocketResolver4)
      "tcp4"
      name1
      name2
      name3
      name4

tests6 :: IO [Test.Framework.Test]
tests6 = do
  name1 <- newTCPAddress6
  name2 <- newTCPAddress6
  name3 <- newTCPAddress6
  name4 <- newTCPAddress6
  return $ transportTestSuite
    (newTCPTransport6 tcpSocketResolver6)
    "tcp6"
    name1
    name2
    name3
    name4


_log :: String
_log = "_test_TCP"
