{-# LANGUAGE OverloadedStrings #-}
module Happstack.Fay where

import Data.Aeson
import Data.Data
import Happstack.Server
import Language.Fay.AJAX
import Language.Fay.Convert

handleCommand :: (Data cmd, Show cmd, Happstack m) =>
                 (cmd -> m Response)
              -> m Response
handleCommand handler =
    do json <- lookBS "json"
       let val = (decode' json)
           mCmd = readFromFay =<< val
       case mCmd of
         Nothing    -> badRequest $ toResponse ("Failed to turn this into a command: " ++ show (val))
         (Just cmd) -> handler cmd

fayResponse :: (Happstack m, Show a) =>
               ResponseType a
            -> m a
            -> m Response
fayResponse _rt m =
    do a <- m
       case showToFay a of
         Nothing -> internalServerError $ toResponse ("showToFay failed to convert response." :: String)
         (Just json) ->
             ok $ toResponseBS "application/json;charset=UTF-8" $ encode json
