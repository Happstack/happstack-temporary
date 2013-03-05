{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, PackageImports  #-}
{- |

The server-side half of a typed AJAX communication channel.

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

In the server, you would then have a route that handles ajax requests such as:

@
    , dir "ajax"     $ handleCommand (commandR acid)
@

@commandR@ would then call functions to handle the various requests:

@
-- | handle an AJAX request
commandR :: AcidState Games
         -> Command
         -> ServerPart Response
commandR acid cmd =
    case cmd of
      (SendGuess guess rt) -> fayResponse rt $ sendGuessC acid guess
      (FetchBoard rt)      -> fayResponse rt $ fetchBoardC acid
@

@commandR@ uses 'fayResponse' to convert the value returned by each
command handler to a valid Fay value. Note that it takes
'ResponseType' argument and passes it to 'fayResponse'. This is how we
ensure that each commend handler is returning the right type.

See also @AJAX@ from the @happstack-client-fay@ package.

-}
module Happstack.Fay where

-- NOTE: we do not really need to use NoImplicitPrelude and
-- PackageImports here since this module only needs things from "base"
-- and we do not compile against "fay-base". However, when debugging
-- things in GHCi, we might have both "base" and "fay-base"
-- loaded. So, using PackageImports just makes things easier.

import "base" Prelude
import Control.Monad.Trans (liftIO)
import Data.Aeson
import "base" Data.Data
import Happstack.Server
import ResponseType
import Fay.Convert

-- | decode the 'cmd' and call the response handler.
--
-- See also: 'fayResponse'
handleCommand :: (Data cmd, Show cmd, Happstack m) =>
                 (cmd -> m Response)
              -> m Response
handleCommand handler =
    do json <- lookBS "json"
       liftIO $ print json
       let val = (decode' json)
           mCmd = readFromFay =<< val
       liftIO $ print val
       liftIO $ print mCmd
       case mCmd of
         Nothing    -> badRequest $ toResponse ("Failed to turn this into a command: " ++ show (val))
         (Just cmd) -> handler cmd

-- | convert the return value to a fay response.
--
fayResponse :: (Happstack m, Show a) =>
               ResponseType a -- ^ used to help the type-checker enforce type safety
            -> m a            -- ^ handler that calculates a response
            -> m Response
fayResponse _rt m =
    do a <- m
       case showToFay a of
         Nothing -> internalServerError $ toResponse ("showToFay failed to convert response." :: String)
         (Just json) ->
             ok $ toResponseBS "application/json;charset=UTF-8" $ encode json
