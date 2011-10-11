{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, TemplateHaskell, TypeFamilies, TypeOperators #-}
-- | NOTE: this must be compiled with -threaded
module Main where

import Acid                              (Acid(..), withAcid)
import Control.Concurrent                (forkIO, killThread)
import Control.Monad                     (msum, mzero)
import Control.Monad.Trans               (liftIO)
import Data.Acid                         (query')
import Data.Text                         (Text)
import qualified Data.Text               as Text
import Happstack.Server                  ( Response(..), ServerPartT, ServerMonad(..), decodeBody
                                         , defaultBodyPolicy, dir, nullDir, ok, nullConf
                                         , seeOther, simpleHTTP, toResponse)
import Pages.AppTemplate                 (appTemplate)
import Pages.Home                        (homePage)
import Happstack.Auth.Core.Auth          (AskAuthState(..))
import Happstack.Auth.Core.ProfileURL    (ProfileURL(P_PickProfile))
import Happstack.Auth.Blaze.Templates    (handleAuth, handleProfile, handleAuthProfile)
import ProfileData                       (ProfileDataURL(CreateNewProfileData), handleProfileData)
import SiteURL                           (SiteURL(..))
import System.Environment                (getArgs)
import System.Exit                       (exitFailure)
import Web.Routes                        (Site(..), PathInfo(..), RouteT(..), setDefault, showURL, nestURL, parseSegments)
import Web.Routes.Happstack              (implSite_)

main :: IO ()
main = 
    do args <- getArgs
       case args of
         [baseURI] ->
             withAcid Nothing $ \acid ->
               do tid <- forkIO $ simpleHTTP nullConf (impl acid (Text.pack baseURI))
                  putStrLn "started. Press <enter> to exit."
                  _ <- getLine
                  killThread tid
         _ -> do putStrLn "usage: demo http://example.org:8000/"
                 exitFailure

impl :: Acid   -- ^ database handle
     -> Text -- ^ base uri 
     -> ServerPartT IO Response
impl acid baseURI = do
    decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
    rq <- askRq
    liftIO $ print rq
    msum [ do r <- implSite_ baseURI (Text.pack "web") (spec acid (Just baseURI))
              case r of
                (Left e) -> liftIO (print e) >> mzero
                (Right r) -> return r
         , dir "dump_auth" $ do authState <- query' (acidAuth acid) AskAuthState 
                                ok $ toResponse (show authState)
         , nullDir >> seeOther "/web/" (toResponse "")
         ]

spec :: Acid         -- ^ database handle
     -> Maybe Text -- ^ authentication realm
     -> Site SiteURL (ServerPartT IO Response)
spec acid realm = 
    setDefault U_HomePage $ 
      Site { handleSite          = \f u -> unRouteT (handle acid realm u) f
           , formatPathSegments  = \u -> (toPathSegments u, [])
           , parsePathSegments   = parseSegments fromPathSegments
           }

handle :: Acid         -- ^ database handle
       -> Maybe Text -- ^ authentication realm
       -> SiteURL      -- ^ url to route
       -> RouteT SiteURL (ServerPartT IO) Response
handle acid@Acid{..} realm url =
    case url of
      U_HomePage          -> do homePage acid
{-
      (U_Auth auth)       -> do onAuthURL <- showURL (U_Profile P_PickProfile)
                                nestURL U_Auth $ handleAuth  acidAuth appTemplate Nothing realm onAuthURL auth
      (U_Profile profile) -> do postPickedURL <- showURL (U_ProfileData CreateNewProfileData)
                                nestURL U_Profile $ handleProfile acidAuth acidProfile appTemplate postPickedURL profile
-}
      (U_AuthProfile authProfileURL) ->
                             do postPickedURL <- showURL (U_ProfileData CreateNewProfileData)
                                nestURL U_AuthProfile $ handleAuthProfile acidAuth acidProfile appTemplate Nothing realm postPickedURL authProfileURL
      (U_ProfileData profileDataURL) ->
                             do handleProfileData acidAuth acidProfile acidProfileData profileDataURL
