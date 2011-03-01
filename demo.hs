{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Main where

import AuthURL
import Control.Concurrent (forkIO, killThread)
import Control.Monad (msum, mzero)
import Control.Monad.Trans (liftIO)
import Data.Data
import Happstack.State
import Happstack.Server
import HSP
import Pages.AppTemplate
import Pages.Home
import Pages.Login
import Pages.Logout
import Pages.Auth
import Pages.Profile
import Profile
import State.Auth
import SiteURL
import ProfileURL
import Web.Routes
import Web.Routes.Happstack          (implSite_)
import Web.Routes.TH
import Web.Routes.MTL

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
                                nestURL U_Auth $ handleAuth realm onAuthURL auth
      (U_Profile profile) -> nestURL U_Profile $ handleProfile profile

handleAuth :: Maybe String -> String -> AuthURL -> RouteT AuthURL (ServerPartT IO) Response
handleAuth realm onAuthURL url =
    case url of
      A_Login                            -> appTemplate "Login"    () loginPage
      A_AddAuth                          -> appTemplate "Add Auth" () addAuthPage
      A_Logout                           -> logoutPage
      (A_OpenIdProvider authMode Google) -> googlePage authMode realm
      (A_OpenIdProvider authMode Yahoo)  -> yahooPage  authMode realm
      (A_OpenId authMode)                -> openIdPage authMode onAuthURL

handleProfile :: ProfileURL -> RouteT ProfileURL (ServerPartT IO) Response
handleProfile url =
    case url of
      P_PickProfile        -> 
          do r <- pickProfile
             case r of
               (Picked {})                -> seeOther "/" (toResponse "/")
               (PickPersonality profiles) -> personalityPicker profiles
               (PickAuthId      authIds)  -> authPicker authIds
                   
                              
      (P_SetAuthId authId) -> 
          do b <- setAuthIdPage authId
             if b
              then seeOther "/" (toResponse "") -- FIXME: don't hardcode destination
              else unauthorized =<< 
                     appTemplate "unauthorized" ()
                        <p>Attempted to set AuthId to <% show $ unAuthId authId %>, but failed because the Identifier is not associated with that AuthId.</p>

