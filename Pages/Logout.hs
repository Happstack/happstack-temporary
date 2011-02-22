{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Pages.Logout where

import AuthURL
import Pages.AppTemplate
import State.Auth
import Happstack.Server
import HSP
import Web.Routes
import Web.Routes.Happstack


logoutPage :: RouteT AuthURL (ServerPartT IO) Response
logoutPage =
    do deleteAuthCookie
       appTemplate "Logout" () 
         <p>You are now logged out. Click <a href=A_Login>here</a> to log in again.</p>
       
