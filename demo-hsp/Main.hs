{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, TemplateHaskell, TypeSynonymInstances, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
-- | NOTE: this must be compiled with -threaded
module Main where

import Acid                              (Acid(..), withAcid)
import Control.Concurrent                (forkIO, killThread)
import Control.Monad                     (liftM, msum, mzero)
import Control.Monad.Trans               (liftIO)
import Control.Applicative
import Data.Acid
import Data.Acid.Advanced
import Happstack.Server.Types
import Happstack.Server                  ( Response(..), ServerPartT, ServerMonad(..), decodeBody
                                         , defaultBodyPolicy, dir, nullDir, ok, validateConf
                                         , seeOther, setValidatorSP, simpleHTTP, toResponse)
import Happstack.Server.HSP.HTML         (defaultTemplate)
import HSP
import qualified HSX.XMLGenerator as HSX
import Pages.Home                        (homePage)
import Happstack.Auth.Core.Auth          (AskAuthState(..))
import Happstack.Auth
import ProfileData                       (ProfileDataURL(CreateNewProfileData), handleProfileData)
import SiteURL                           (SiteURL(..))
import System.Environment                (getArgs)
import System.Exit                       (exitFailure)
import Web.Routes                        (Site(..), PathInfo(..), RouteT(..), setDefault, showURL, nestURL, parseSegments)
import Web.Routes.Happstack              (implSite_)
import Web.Routes.XMLGenT                ()
import Data.Text
import Text.Blaze.Html                   (Html)
import Text.Blaze.Html.Renderer.String   (renderHtml)

-- the key to using happstack-authenticate with HSP is simple. First
-- you need to be able to embed Html in your HSP monad like this:
instance (Functor m, Monad m) => EmbedAsChild (RouteT url m) Html where
    asChild html = asChild (CDATA False (renderHtml html))

-- then you just provide a simple function like this to pass to the happstack-authenticate code
defaultTemplate' :: (XMLGenerator m, EmbedAsChild m Html, HSX.XMLType m ~ XML) => String -> Html -> Html -> m Response
defaultTemplate' t h b = liftM toResponse (defaultTemplate t h b)

myConf :: Conf
myConf = validateConf { port      = 8080 }

main :: IO ()
main =
    do args <- getArgs
       case args of
         [baseURI] ->
             withAcid Nothing $ \acid ->
               do tid <- forkIO $ simpleHTTP myConf (setValidatorSP printResponse $ impl acid (pack baseURI))
                  putStrLn "started. Press <enter> to exit."
                  _ <- getLine
                  killThread tid
         _ -> do putStrLn "usage: demo http://example.org:8000/"
                 exitFailure

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

impl :: Acid -> Text -> ServerPartT IO Response
impl acid baseURI = do
    decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
    rq <- askRq
    liftIO $ print rq
    msum [ do r <- implSite_ baseURI (pack "web") (spec acid (Just baseURI))
              case r of
                (Left e) -> liftIO (print e) >> mzero
                (Right r) -> return r
         , dir "dump_auth" $ do authState <- query' (acidAuth acid) AskAuthState
                                ok $ toResponse (show authState)
         , nullDir >> seeOther (pack "/web") (toResponse (pack ""))
         ]

spec :: Acid -> Maybe Text -> Site SiteURL (ServerPartT IO Response)
spec acid realm =
    setDefault U_HomePage $
      Site { handleSite          = \f u -> unRouteT (handle acid realm u) f
           , formatPathSegments  = \u -> (toPathSegments u, [])
           , parsePathSegments   = parseSegments fromPathSegments
           }

-- TODO: use urlTemplate
handle :: Acid -> Maybe Text -> SiteURL -> RouteT SiteURL (ServerPartT IO) Response
handle acid@Acid{..} realm url =
    case url of
      U_HomePage          -> homePage acid

      (U_Auth auth)       -> do onAuthURL <- showURL (U_Profile P_PickProfile)
                                nestURL U_Auth $ handleAuth acidAuth defaultTemplate' Nothing realm onAuthURL auth
      (U_Profile profile) -> do postPickedURL <- showURL (U_ProfileData CreateNewProfileData)
                                nestURL U_Profile $ handleProfile acidAuth acidProfile defaultTemplate' postPickedURL profile

      (U_ProfileData profileDataURL) ->
                             do handleProfileData acidAuth acidProfile acidProfileData profileDataURL
