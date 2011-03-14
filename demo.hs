{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Main where

import Control.Applicative (Alternative(..))
import Control.Concurrent (forkIO, killThread)
import Control.Monad (liftM, msum, mzero)
import Control.Monad.Trans (liftIO)
import Data.Data
import Happstack.State
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
import SiteURL
import Types ()
import Web.Routes
import Web.Routes.Happstack          (implSite_)
import Web.Routes.XMLGenT ()
import Web.Routes.TH
import Web.Routes.MTL

defaultTemplate' :: (XMLGenerator m, EmbedAsChild m h, EmbedAsChild m b, HSX.XML m ~ XML) => String -> h -> b -> m Response
defaultTemplate' t h b = liftM toResponse (defaultTemplate t h b)

data DemoState = DemoState
      deriving (Eq, Read, Show, Data, Typeable)
$(deriveSerialize ''DemoState)
instance Version DemoState

instance Component DemoState where
    type Dependencies DemoState = AuthState :+: ProfileState :+: End
    initialValue = DemoState

$(mkMethods ''DemoState [])

main :: IO ()
main = 
    do state <- startSystemState (Proxy :: Proxy DemoState)
       tid <- forkIO $ simpleHTTP validateConf (setValidatorSP printResponse $ impl "http://www.n-heptane.com:8000/")
       putStrLn "started."
       waitForTermination
       killThread tid
       shutdownSystem state

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

impl :: String -> ServerPartT IO Response
impl baseURI = do
    decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
    rq <- askRq
    liftIO $ print rq
    msum [ do r <- implSite_ baseURI "web/" (spec (Just "http://*.n-heptane.com:8000/"))
              case r of
                (Left e) -> liftIO (print e) >> mzero
                (Right r) -> return r
         , dir "dump_auth" $ do authState <- query AskAuthState 
                                ok $ toResponse (show authState)
         , nullDir >> seeOther "/web/" (toResponse "")
         ]

spec :: Maybe String -> Site SiteURL (ServerPartT IO Response)
spec realm = 
    setDefault U_HomePage $ 
      Site { handleSite          = \f u -> unRouteT (handle realm u) f
           , formatPathSegments  = \u -> (toPathSegments u, [])
           , parsePathSegments   = parseSegments fromPathSegments
           }

handle :: Maybe String -> SiteURL -> RouteT SiteURL (ServerPartT IO) Response
handle realm url =
    case url of
      U_HomePage          -> homePage
      (U_Auth auth)       -> do onAuthURL <- showURL (U_Profile P_PickProfile)
                                nestURL U_Auth $ handleAuth defaultTemplate' realm onAuthURL auth
      (U_Profile profile) -> nestURL U_Profile $ handleProfile defaultTemplate' profile


-- handleProfile :: ProfileURL -> RouteT ProfileURL (ServerPartT IO) Response

