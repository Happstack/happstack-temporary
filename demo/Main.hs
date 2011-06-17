{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, TemplateHaskell, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
-- | NOTE: this must be compiled with -threaded
module Main where

import Acid
import Control.Applicative (Alternative(..))
import Control.Concurrent (forkIO, killThread)
import Control.Monad (liftM, msum, mzero)
import Control.Monad.Trans (liftIO)
import Data.Acid (query')
import Data.Data
import Happstack.Server
import Happstack.Server.HSP.HTML (defaultTemplate)
import HSP
import qualified HSX.XMLGenerator as HSX
-- import Pages.AppTemplate
import Pages.Home
import Happstack.Auth.Core.Auth
import Happstack.Auth.Core.AuthParts
import Happstack.Auth.Core.AuthURL
import Happstack.Auth.Core.Profile
import Happstack.Auth.Core.ProfileParts
import Happstack.Auth.Core.ProfileURL
import Happstack.Auth.HSP.Login
import ProfileData
import SiteURL
import System.Environment
import Web.Routes
import Web.Routes.Happstack          (implSite_)
import Web.Routes.XMLGenT ()
import Web.Routes.TH

defaultTemplate' :: (XMLGenerator m, EmbedAsChild m h, EmbedAsChild m b, HSX.XML m ~ XML) => String -> h -> b -> m Response
defaultTemplate' t h b = liftM toResponse (defaultTemplate t h b)

main :: IO ()
main = 
    do [baseURI] <- getArgs
       withAcid Nothing $ \acid ->
           do tid <- forkIO $ simpleHTTP validateConf (setValidatorSP printResponse $ impl acid baseURI)
              putStrLn "started. Press <enter> to exit."
              getLine
              killThread tid

printResponse :: Response -> IO Response
printResponse res =
    do putStrLn $ showResponse res ""
       return res
    where
      showResponse res@Response{}  =
          showString   "================== Response ================" .
          showString "\nrsCode      = " . shows      (rsCode res)     .
          showString "\nrsHeaders   = " . shows      (rsHeaders res)  .
          showString "\nrsFlags     = " . shows      (rsFlags res)    .
          showString "\nrsBody      = " . shows      (rsBody res)     .
          showString "\nrsValidator = " . shows      (rsValidator res)
      showResponse res@SendFile{}  =
          showString   "================== Response ================" .
          showString "\nrsCode      = " . shows      (rsCode res)     .
          showString "\nrsHeaders   = " . shows      (rsHeaders res)  .
          showString "\nrsFlags     = " . shows      (rsFlags res)    .
          showString "\nrsValidator = " . shows      (rsValidator res).
          showString "\nsfFilePath  = " . shows      (sfFilePath res) .
          showString "\nsfOffset    = " . shows      (sfOffset res)   .
          showString "\nsfCount     = " . shows      (sfCount res)

impl :: Acid -> String -> ServerPartT IO Response
impl acid baseURI = do
    decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
    rq <- askRq
    liftIO $ print rq
    msum [ do r <- implSite_ baseURI "web/" (spec acid (Just baseURI))
              case r of
                (Left e) -> liftIO (print e) >> mzero
                (Right r) -> return r
         , dir "dump_auth" $ do authState <- query' (acidAuth acid) AskAuthState 
                                ok $ toResponse (show authState)
         , nullDir >> seeOther "/web/" (toResponse "")
         ]

spec :: Acid -> Maybe String -> Site SiteURL (ServerPartT IO Response)
spec acid realm = 
    setDefault U_HomePage $ 
      Site { handleSite          = \f u -> unRouteT (handle acid realm u) f
           , formatPathSegments  = \u -> (toPathSegments u, [])
           , parsePathSegments   = parseSegments fromPathSegments
           }

-- TODO: use urlTemplate
handle :: Acid -> Maybe String -> SiteURL -> RouteT SiteURL (ServerPartT IO) Response
handle acid@Acid{..} realm url =
    case url of
      U_HomePage          -> homePage acid
      (U_Auth auth)       -> do onAuthURL <- showURL (U_Profile P_PickProfile)
                                nestURL U_Auth $ handleAuth acidAuth defaultTemplate' realm onAuthURL auth
      (U_Profile profile) -> do postPickedURL <- showURL (U_ProfileData CreateNewProfileData)
                                nestURL U_Profile $ handleProfile acidAuth acidProfile defaultTemplate' postPickedURL profile
      (U_ProfileData profileDataURL) ->
                             do handleProfileData acidAuth acidProfile acidProfileData profileDataURL
