{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Pages.Home where

import AuthURL
import Happstack.Server
import HSP
import Pages.AppTemplate
import SiteURL
import Profile
import Web.Routes
import Types

homePage :: RouteT SiteURL (ServerPartT IO) Response
homePage =
    do mUserId <- getUserId
       case mUserId of
         Nothing -> 
             appTemplate "not logged in." ()
               <div>
                <p>You can login <a href=(U_Auth A_Login)>here</a>.</p>
               </div>
         (Just (UserId uid)) ->
             appTemplate "logged in." ()
               <div>
                <p>You are logged in as <% show uid %>. You can logout <a href=(U_Auth A_Logout)>here</a>.</p>
               </div>
