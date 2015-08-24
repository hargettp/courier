-----------------------------------------------------------------------------
-- |
-- Module      :  EchoServer
-- Copyright   :  (c) Phil Hargett 2015
-- License     :  MIT (see LICENSE file)
--
-- Maintainer  :  phil@haphazardhouse.net
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- Simple echo server for testing out courier.
--
-----------------------------------------------------------------------------

module Main where

-- local imports

import Network.Endpoints
import Network.RPC
import Network.Transport.Sockets.TCP

-- external imports

import Control.Exception

import Data.Serialize

import System.Environment
import System.IO

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

main :: IO ()
main = do
  [nameStr] <- getArgs
  let server = Name nameStr
  endpoint <- newEndpoint
  withTransport (newTCPTransport4 tcpSocketResolver4) $ \transport ->
    withEndpoint transport endpoint $
      withBinding transport endpoint server $ do
        hPutStrLn stdout $ "Started echo server on " ++ (show server)
        finally (echo endpoint server)
          (hPutStrLn stdout $ "\nStopped echo server on " ++ (show server))

echo :: Endpoint -> Name -> IO ()
echo endpoint server = do
  (bytes,reply) <- hear endpoint server "echo"
  case decode bytes of
    Left _ -> error "Could not decode message"
    Right text -> do
      hPutStrLn stdout text
      reply $ encode text
      echo endpoint server
