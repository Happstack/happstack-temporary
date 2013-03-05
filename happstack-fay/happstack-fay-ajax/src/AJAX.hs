{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude, PackageImports #-}
{- |

client-side half of a typed AJAX communication channel.

To use this library, you could start by defining a type in a file that
can be shared between the Haskell Server and Fay client. For example:

@
    data Command
        = SendGuess Guess (ResponseType (Maybe Row))
        | FetchBoard (ResponseType (Maybe Board))
        deriving (Read, Show, Data, Typeable)
    instance Foreign Command
@

The 'ResponseType' argument specifies what type each command should
return. Using GADTs would be cleaner, but Fay does not support GADTs
yet.

To execute a remote function we use the 'call' function:

@
      call "/ajax" FetchBoard $ \mboard -> ...
@

Due to the single-threaded nature of Javascript, we do not want to
block until the 'call' returns a value, so we perform the AJAX request
asynchronously. The third argument to 'call' is the callback function
to run when the response is received.

-}
module AJAX where

import "fay-base" Data.Data
import FFI
import JQuery
import ResponseType
import "fay-base" Prelude

-- | Asynchronously call a command
--
-- Note: if the server returns 404 or some other non-success exit
-- code, the callback function will never be run.
--
-- This function is just a wrapper around 'ajaxCommand' which uses the
-- 'ResponseType res' phantom-typed parameter for added type safety.
call :: String                    -- ^ URL to 'POST' AJAX request to
     -> (ResponseType res -> cmd) -- ^ AJAX command to send to server
     -> (res -> Fay ())           -- ^ callback function to handle response
     -> Fay ()
call uri f g = ajaxCommand uri (f ResponseType) g

-- | Run the AJAX command. (internal)
--
-- You probably want to use 'call' which provides additional
-- type-safety.
--
-- Note: if the server returns 404 or some other non-success exit
-- code, the callback function will never be run.
--
-- see also: 'call'
ajaxCommand :: String -> Automatic cmd -> (Automatic res -> Fay ()) -> Fay ()
ajaxCommand =
    ffi "jQuery['ajax']({'url': %1, 'type': 'POST', 'data': { 'json' : JSON.stringify(%2) }, 'dataType': 'json', 'success' : %3 })"
