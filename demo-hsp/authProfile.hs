-- | simple demo of using Happstack.Auth.HSP.AuthProfile
module Main where

import Control.Exception (bracket)
import Control.Monad     (mzero, msum)
import Data.Acid         (AcidState, IsAcidic, openAcidState, createCheckpointAndClose)
import Data.Data         (Typeable)
import Happstack.Server  (ServerPart, Response, decodeBody, defaultBodyPolicy, nullConf, ok, simpleHTTP, seeOther, toResponse)
import Happstack.Auth.Core.Auth       (AuthState, initialAuthState, getAuthId)
import Happstack.Auth.Core.Profile    (ProfileState, initialProfileState)
import Happstack.Auth.HSP.AuthProfile (authProfileHandler)
import Happstack.Server.HSP.HTML      (defaultTemplate)

defaultTemplate' t h b = fmap toResponse $ defaultTemplate t h b

main :: IO ()
main =
    withAcid initialAuthState $ \acidAuth ->
    withAcid initialProfileState $ \acidProfile ->
      simpleHTTP nullConf (handler acidAuth acidProfile)

withAcid :: (Typeable st, IsAcidic st) => st -> (AcidState st -> IO a) -> IO a
withAcid initialState fn =
    bracket (openAcidState initialState) createCheckpointAndClose fn

handler :: AcidState AuthState -> AcidState ProfileState -> ServerPart Response
handler acidAuth acidProfile =
    do decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
       msum [ authProfileHandler "http://localhost:8000" "/ap/" acidAuth acidProfile defaultTemplate' Nothing Nothing "/"
            , requireAuth acidAuth
            , ok $ toResponse "you made it!"
            ]

requireAuth :: AcidState AuthState -> ServerPart Response
requireAuth acidAuth =
    do ma <- getAuthId acidAuth
       case ma of
         Just _ -> mzero
         Nothing -> seeOther "/ap/auth/login" (toResponse "")
