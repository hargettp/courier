-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Transport.Internal
-- Copyright   :  (c) Phil Hargett 2013
-- License     :  MIT (see LICENSE file)
-- 
-- Maintainer  :  phil@haphazardhouse.net
-- Stability   :  experimental
-- Portability :  non-portable (uses STM)
--
-- Internal definitions used for implementing transports.
-----------------------------------------------------------------------------

module Network.Transport.Internal (
    catchExceptions
)

where

-- local imports
-- external imports

import Control.Exception

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-|
Silently ignore 'BlockedIndefinitelyOnSTM' and 'ThreadKilled' exceptions, as
most cases in which they occur are benign and not worth the noise.  Use with 
caution, as this function could mask undelrying design issues.
-}
catchExceptions :: IO () -> (SomeException -> IO ()) -> IO ()
catchExceptions blk handler = do
    catch (do
        catch (catch blk ignoreBlocked)
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
