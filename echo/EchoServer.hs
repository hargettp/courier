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
  let name = Name "localhost:9001"
  transport <- newTCPTransport4 tcpSocketResolver4
  withEndpoint transport $ \endpoint ->
    withBinding transport endpoint name $ do
      hPutStrLn stdout "Started echo server"
      finally (echo endpoint name)
        (hPutStrLn stdout "\nStopped echo server")

echo :: Endpoint -> Name -> IO ()
echo endpoint name = do
  (bytes,reply) <- hear endpoint name "echo"
  case decode bytes of
    Left _ -> error "Could not decode message"
    Right text -> do
      hPutStrLn stdout text
      reply $ encode text
      echo endpoint name
