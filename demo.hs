{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Main where

import AuthURL
import Control.Monad (msum, mzero)
import Control.Monad.Trans (liftIO)
import Happstack.Server
import HSP
import Pages.AppTemplate
import Pages.Login
import State.Auth
import SiteURL
import ProfileURL
import Web.Routes
import Web.Routes.Happstack          (implSite_)
import Web.Routes.TH
import Web.Routes.MTL

main :: IO ()
main = simpleHTTP nullConf (impl "http://www.n-heptane.com:8000/")

impl :: String -> ServerPartT IO Response
impl baseURI = 
    msum [ do r <- implSite_ baseURI "web/" spec
              case r of
                (Left e) -> liftIO (print e) >> mzero
                (Right r) -> return r
         , nullDir >> seeOther "/web/" (toResponse "")
         ]

spec :: Site SiteURL (ServerPartT IO Response)
spec = 
    setDefault U_HomePage $ 
      Site { handleSite          = \f u -> unRouteT (handle u) f
           , formatPathSegments  = \u -> (toPathSegments u, [])
           , parsePathSegments   = parseSegments fromPathSegments
           }

handle :: SiteURL -> RouteT SiteURL (ServerPartT IO) Response
handle U_HomePage    = homePage
handle (U_Auth auth) = nestURL U_Auth $ handleAuth auth
handle (U_Profile profile) = nestURL U_Profile $ handleProfile profile

handleAuth :: AuthURL -> RouteT AuthURL (ServerPartT IO) Response
handleAuth A_Login = loginPage
handleAuth (A_OpenIdProvider Google) = googlePage (Just "http://*.n-heptane.com/")

handleProfile :: ProfileURL -> RouteT ProfileURL (ServerPartT IO) Response
handleProfile P_PickProfile = pickProfile

homePage :: RouteT SiteURL (ServerPartT IO) Response
homePage = 
    appTemplate "this page rocks." ()
      <div>
       <p>You can login <a href=(U_Auth A_Login)>here</a>.</p>
      </div>

loginPage :: RouteT AuthURL (ServerPartT IO) Response
loginPage =
    appTemplate "login" () $
      <ol>
       <li><a href=(A_OpenIdProvider Google)>Google</a></li>
      </ol>
