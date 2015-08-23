module Main where

-- local imports

import Network.Endpoints
import Network.RPC
import Network.Transport.Sockets.TCP

-- external imports

import Control.Exception

import Data.Serialize

import System.IO

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

main :: IO ()
main = do
  let server = Name "localhost:9001"
      client = Name "localhost:9002"
  endpoint <- newEndpoint
  withTransport (newTCPTransport4 tcpSocketResolver4) $ \transport ->
    withEndpoint transport endpoint $
      withName endpoint client $
        withConnection transport endpoint server $ do
          hPutStrLn stdout "Started echo client"
          finally (echo endpoint server client)
            (hPutStrLn stdout "\nStopped echo client")

echo :: Endpoint -> Name -> Name -> IO ()
echo endpoint server client = do
  text <- hGetLine stdin
  let cs = newCallSite endpoint client
  response <- call cs server "echo" $ encode text
  case decode response of
    Left _ -> error "Could not decode message"
    Right responseText -> do
      hPutStrLn stdout responseText
      echo endpoint server client
