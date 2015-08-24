-----------------------------------------------------------------------------
-- |
-- Module      :  EchoClient
-- Copyright   :  (c) Phil Hargett 2015
-- License     :  MIT (see LICENSE file)
--
-- Maintainer  :  phil@haphazardhouse.net
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- Simple echo client for testing out courier.
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
  [serverNameStr,clientNameStr] <- getArgs
  let server = Name serverNameStr
      client = Name clientNameStr
  endpoint <- newEndpoint
  withTransport (newTCPTransport4 tcpSocketResolver4) $ \transport ->
    withEndpoint transport endpoint $
      withName endpoint client $
        withConnection transport endpoint server $ do
          hPutStrLn stdout $ "Started echo client on " ++ (show client) ++ " for " ++ (show server)
          finally (echo endpoint server client)
            (hPutStrLn stdout $ "\nStopped echo client on " ++ (show client) ++ " for " ++ (show server))

echo :: Endpoint -> Name -> Name -> IO ()
echo endpoint server client = do
  text <- hGetLine stdin
  let cs = newCallSite endpoint client
  response <- call cs server "echo" $ encode text
  case decode response of
    Left _ -> error "Could not decode message"
    Right responseText -> do
      hPutStrLn stdout $ "> " ++ responseText
      echo endpoint server client
