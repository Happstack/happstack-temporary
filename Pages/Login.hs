{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Pages.Login where

import AuthURL
import Control.Applicative        (Alternative, (<*>), (<$>), (<*), optional)
import Control.Monad              (replicateM)
import Control.Monad.Trans        (MonadIO(liftIO))
import Data.Maybe                 (mapMaybe)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import Data.Text                  (Text)
import Data.Time.Clock            (getCurrentTime)
import Happstack.Server           (CookieLife(Session), Response, ServerMonad(..), FilterMonad(..), Happstack, ServerPartT, addCookie, escape, internalServerError, lookCookieValue, lookPairs, mkCookie, seeOther, toResponse, unauthorized)
import Happstack.Server.HSP.HTML  (XML)
import Happstack.State            (query, update)
import HSP                        (Attr(..), EmbedAsAttr(..), EmbedAsChild(..), XMLGenT, genElement, unXMLGenT)
import HSP.ServerPartT()
import State.Auth
import Pages.Auth
import Pages.AppTemplate
import Pages.Profile
import Profile
import ProfileURL
import Text.Digestive
import Text.Digestive.HSP.Html4
import Types (UserId(..))
import Web.Authenticate.OpenId    (Identifier, authenticate, getForwardUrl)
import Web.Authenticate.OpenId.Providers (google, yahoo, livejournal, myspace)
import Web.Routes                 (RouteT, ShowURL, showURL, URL)
import Web.Routes.XMLGenT

-- * AuthURL stuff

loginPage :: XMLGenT (RouteT AuthURL (ServerPartT IO)) XML
loginPage =
      <ol>
       <li><a href=(A_OpenIdProvider LoginMode Google)     >Login</a> with your Google</li>
       <li><a href=(A_OpenIdProvider LoginMode Yahoo)      >Login</a> with your Yahoo Account</li>
       <li><a href=(A_OpenIdProvider LoginMode LiveJournal)>Login</a> with your Live Journal Account</li>
       <li><a href=(A_OpenIdProvider LoginMode Myspace)    >Login</a> with your Myspace Account</li>
       <li><a href=(A_OpenIdProvider LoginMode Generic)    >Login</a> with your OpenId Account</li>
      </ol>

addAuthPage :: XMLGenT (RouteT AuthURL (ServerPartT IO)) XML
addAuthPage =
      <ol>
       <li><a href=(A_OpenIdProvider AddIdentifierMode Google)     >Add</a> your Google</li>
       <li><a href=(A_OpenIdProvider AddIdentifierMode Yahoo)      >Add</a> your Yahoo Account</li>
       <li><a href=(A_OpenIdProvider AddIdentifierMode LiveJournal)>Add</a> your Live Journal Account</li>
       <li><a href=(A_OpenIdProvider AddIdentifierMode Myspace)    >Add</a> your Myspace Account</li>
       <li><a href=(A_OpenIdProvider AddIdentifierMode Generic)    >Add</a> your OpenId Account</li>
      </ol>

authPicker :: Set AuthId -> RouteT ProfileURL (ServerPartT IO) Response
authPicker authIds =
    appTemplate "Pick An Auth" ()
                <div>
                 <ul><% mapM auth (Set.toList authIds) %></ul>
                </div>
    where
      auth authId =
          <li><a href=(P_SetAuthId authId)><% show authId %></a></li> -- FIXME: give a more informative view. 

personalityPicker :: Set Profile -> RouteT ProfileURL (ServerPartT IO) Response
personalityPicker profiles =
    appTemplate "Pick A Personality" ()
                <div>
                 <ul><% mapM personality (Set.toList profiles) %></ul>
                </div>
    where
      personality profile =
          <li><a href=(P_SetPersonality (userId profile))><% nickName profile %></a></li>

googlePage :: (Happstack m, ShowURL m, URL m ~ AuthURL) => 
              AuthMode     -- ^ authentication mode
           -> Maybe String -- ^ realm
           -> m Response
googlePage authMode realm =
    do openIdUrl <- showURL (A_OpenId authMode)
       gotoURL <- liftIO $ getForwardUrl google openIdUrl realm []
       seeOther gotoURL (toResponse gotoURL)              

yahooPage :: (Happstack m, ShowURL m, URL m ~ AuthURL) => 
              AuthMode     -- ^ authentication mode
           -> Maybe String -- ^ realm
           -> m Response
yahooPage authMode realm =
    do openIdUrl <- showURL (A_OpenId authMode)
       gotoURL <- liftIO $ getForwardUrl yahoo openIdUrl realm []
       seeOther gotoURL (toResponse gotoURL)              


