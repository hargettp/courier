module Network.Transport.Internal (
    catchExceptions
)

where

import Control.Exception

{-|
Silently ignore 'BlockedIndefinitelyOnSTM' and 'ThreadKilled' exceptions, as
most cases in which they occur are benign and not worth the noise.  Use with 
caution, as this function could mask undelrying design issues.
-}
catchExceptions :: IO () -> (SomeException -> IO ()) -> IO ()
catchExceptions block handler = do
    catch (do
        catch (catch block ignoreBlocked)
            ignoreThreadKilled)
        handler
    where
        ignoreBlocked :: BlockedIndefinitelyOnSTM -> IO ()
        ignoreBlocked _ = return ()
        ignoreThreadKilled :: AsyncException -> IO ()
        ignoreThreadKilled e =
            case e of
                ThreadKilled -> return ()
                _ -> handler $ SomeException e
