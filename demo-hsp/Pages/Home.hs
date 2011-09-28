{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Pages.Home where

import Acid
import Data.Acid
import Data.Maybe
import qualified Data.Text as Text
import Happstack.Server
import HSP
import Pages.AppTemplate
import SiteURL
import Happstack.Auth.Core.Auth    (AuthState)
import Happstack.Auth.Core.AuthURL
import Happstack.Auth.Core.Profile
import ProfileData
import Web.Routes

homePage :: Acid -> RouteT SiteURL (ServerPartT IO) Response
homePage Acid{..} =
    do mUserId <- getUserId acidAuth acidProfile
       case mUserId of
         Nothing -> 
             appTemplate "not logged in." ()
               <div>
                <p>You can login <a href=(U_Auth A_Login)>here</a>.</p>
               </div>
         (Just uid) -> do 
             mpd <- query' acidProfileData (AskProfileData uid)
             appTemplate "logged in." ()
               <div>
                <p>You are logged in as <% show uid %>.</p> 
                <p>You can logout <a href=(U_Auth A_Logout)>here</a>.</p>
                <p>You can add an additional auth method <a href=(U_Auth A_AddAuth)>here</a>.</p>
                <p>Your message is: <% fromMaybe (Text.pack "profile data missing.") (fmap profileMsg mpd) %></p>
               </div>
