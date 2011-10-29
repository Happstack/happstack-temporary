{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, TemplateHaskell, TypeFamilies, TypeOperators #-}
-- | This app demonstrates basic usage of the happstack-authenticate library.
module Main where

import Acid                              (Acid(..), withAcid)
import Control.Concurrent                (forkIO, killThread)
import Control.Monad                     (msum, mzero)
import Control.Monad.Trans               (liftIO)
import Data.Acid                         (query')
import Data.Text                         (Text)
import qualified Data.Text               as Text
import Happstack.Server                  ( Response(..), ServerPartT, decodeBody
                                         , defaultBodyPolicy, dir, nullDir, ok, nullConf
                                         , seeOther, simpleHTTP, toResponse)
import Pages.AppTemplate                 (appTemplate)
import Pages.Home                        (homePage)
import Happstack.Auth.Core.Auth          (AskAuthState(..))
import Happstack.Auth.Blaze.Templates    (handleAuthProfile)
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
               do tid <- forkIO $ simpleHTTP nullConf (route acid (Text.pack baseURI))
                  putStrLn "started. Press <enter> to exit."
                  _ <- getLine
                  killThread tid
         _ -> do putStrLn "usage: demo http://example.org:8000/"
                 exitFailure


-- | map an incoming 'Request' a handler
route :: Acid -- ^ database handle
      -> Text -- ^ base uri
      -> ServerPartT IO Response
route acid baseURI = do
    decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
    msum [ -- this is the handler that actually deals with authentication / profiles.
           --
           -- If you do not wish to use web-routes in the rest of your
           -- application, you could just call the
           -- happstack-authenticate functions like this, and add the
           -- rest of your handlers the normal way.
           do e <- implSite_ baseURI (Text.pack "web") (spec acid (Just baseURI))
              case e of
                (Left err) -> liftIO (print err) >> mzero
                (Right resp) -> return resp
           -- a little debug handler which dumps the auth information.
           -- clearly not good for a real app
         , dir "dump_auth" $ do authState <- query' (acidAuth acid) AskAuthState 
                                ok $ toResponse (show authState)
         , nullDir >> seeOther "/web/" (toResponse "")
         ]

-- | route 'SiteURL' to the appropriate handlers
routeSiteURL :: Acid         -- ^ database handle
             -> Maybe Text -- ^ authentication realm
             -> SiteURL      -- ^ url to route
             -> RouteT SiteURL (ServerPartT IO) Response
routeSiteURL acid@Acid{..} realm url =
    case url of
      U_HomePage          -> do homePage acid
      (U_AuthProfile authProfileURL) ->
                             do postPickedURL <- showURL (U_ProfileData CreateNewProfileData)
                                nestURL U_AuthProfile $ handleAuthProfile acidAuth acidProfile appTemplate Nothing realm postPickedURL authProfileURL
      (U_ProfileData profileDataURL) ->
                             do handleProfileData acidAuth acidProfile acidProfileData profileDataURL

spec :: Acid       -- ^ database handle
     -> Maybe Text -- ^ authentication realm
     -> Site SiteURL (ServerPartT IO Response)
spec acid realm = 
    setDefault U_HomePage $ 
      Site { handleSite          = \f u -> unRouteT (routeSiteURL acid realm u) f
           , formatPathSegments  = \u -> (toPathSegments u, [])
           , parsePathSegments   = parseSegments fromPathSegments
           }
