module HelloWorld (
    main
) where

 -- Just import this package to access the primary APIs
import Network.Endpoints

 -- A specific transport is necessary, however
import Network.Transport.TCP

-- Needed for serialization
import Data.Serialize

main :: IO ()
main = do
    let name1 = "endpoint1"
        name2 = "endpoint2"
        resolver = resolverFromList [(name1,"localhost:2000"),
                                (name2,"localhost:2001")]
    transport <- newTCPTransport resolver
    endpoint1 <- newEndpoint [transport]
    endpoint2 <- newEndpoint [transport]
    Right () <- bindEndpoint endpoint1 name1
    Right () <- bindEndpoint endpoint2 name2
    sendMessage_ endpoint1 name2 $ encode "hello world!"
    msg <- receiveMessage endpoint2
    let Right txt = decode msg
        in print (txt :: String)
    Right () <- unbindEndpoint endpoint1 name1
    Right () <- unbindEndpoint endpoint2 name2
    shutdown transport