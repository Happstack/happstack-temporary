{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Happstack.Auth.HSP.Login where

import Control.Applicative        (Alternative, (<*>), (<$>), (<*), optional)
import Control.Monad              (replicateM, mplus)
import Control.Monad.Trans        (MonadIO(liftIO))
import Data.Maybe                 (mapMaybe)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import Data.Text                  (Text)
import Data.Time.Clock            (getCurrentTime)
import Happstack.Auth.Core.Auth
import Happstack.Auth.Core.AuthParts
import Happstack.Auth.Core.AuthURL
import Happstack.Auth.Core.ProfileURL
import Happstack.Auth.Core.Profile
import Happstack.Auth.Core.ProfileParts
import Happstack.Server           -- (CookieLife(Session), Response, ServerMonad(..), FilterMonad(..), Input(..), Happstack, ServerPartT, addCookie, escape, internalServerError, lookCookieValue, lookPairs, mkCookie, seeOther, toResponse, unauthorized)
import Happstack.Server.HSP.HTML  (XML)
import Happstack.State            (query, update)
import HSP                        (Attr(..), EmbedAsAttr(..), EmbedAsChild(..), XMLGenT(..), XMLGenerator, genElement, unXMLGenT)
import HSP.ServerPartT()
import qualified HSX.XMLGenerator as HSX
import Happstack.Auth.HSP.FormPart
import Text.Digestive
import Text.Digestive.Forms.Happstack ()
import Text.Digestive.HSP.Html4
import Web.Authenticate.OpenId    (Identifier, authenticate, getForwardUrl)
import Web.Authenticate.OpenId.Providers (google, yahoo, livejournal, myspace)
import Web.Routes                 (RouteT, ShowURL, showURL, showURLParams, nestURL, URL)
import Web.Routes.XMLGenT
import Types ()

-- * AuthURL stuff
logoutPage :: (XMLGenerator m, Alternative m, Happstack m, ShowURL m, URL m ~ AuthURL, EmbedAsAttr m (Attr String AuthURL)) => XMLGenT m (HSX.XML m)
logoutPage =
    do deleteAuthCookie
       <p>You are now logged out. Click <a href=A_Login>here</a> to log in again.</p>


loginPage :: (XMLGenerator m, EmbedAsAttr m (Attr String AuthURL)) => XMLGenT m (HSX.XML m)
loginPage =
      <ol>
       <li><a href=(A_OpenIdProvider LoginMode Google)     >Login</a> with your Google</li>
       <li><a href=(A_OpenIdProvider LoginMode Yahoo)      >Login</a> with your Yahoo Account</li>
       <li><a href=(A_OpenIdProvider LoginMode LiveJournal)>Login</a> with your Live Journal Account</li>
       <li><a href=(A_OpenIdProvider LoginMode Myspace)    >Login</a> with your Myspace Account</li>
       <li><a href=(A_OpenIdProvider LoginMode Generic)    >Login</a> with your OpenId Account</li>
      </ol>


addAuthPage :: (XMLGenerator m, EmbedAsAttr m (Attr String AuthURL)) => XMLGenT m (HSX.XML m)
addAuthPage =
      <ol>
       <li><a href=(A_OpenIdProvider AddIdentifierMode Google)     >Add</a> your Google</li>
       <li><a href=(A_OpenIdProvider AddIdentifierMode Yahoo)      >Add</a> your Yahoo Account</li>
       <li><a href=(A_OpenIdProvider AddIdentifierMode LiveJournal)>Add</a> your Live Journal Account</li>
       <li><a href=(A_OpenIdProvider AddIdentifierMode Myspace)    >Add</a> your Myspace Account</li>
       <li><a href=(A_OpenIdProvider AddIdentifierMode Generic)    >Add</a> your OpenId Account</li>
      </ol>

authPicker :: Set AuthId -> XMLGenT (RouteT ProfileURL (ServerPartT IO)) XML
authPicker authIds =
    <div>
     <ul><% mapM auth (Set.toList authIds) %></ul>
    </div>
    where
      auth authId =
          <li><a href=(P_SetAuthId authId)><% show authId %></a></li> -- FIXME: give a more informative view. 

personalityPicker :: Set Profile -> XMLGenT (RouteT ProfileURL (ServerPartT IO)) XML
personalityPicker profiles =
    <div>
     <ul><% mapM personality (Set.toList profiles) %></ul>
    </div>
    where
      personality profile =
          <li><a href=(P_SetPersonality (userId profile))><% nickName profile %></a></li>


type PageTemplate x = String -> () -> (XMLGenT x (HSX.XML x)) -> XMLGenT x Response
type PageTemplate' x = String -> () -> (XMLGenT x (HSX.XML x)) -> x Response

providerPage appTemplate provider =
    case provider of
      Google      -> googlePage
      Yahoo       -> yahooPage
      LiveJournal -> liveJournalPage appTemplate

googlePage :: (Happstack m, ShowURL m, URL m ~ AuthURL) =>
              AuthURL
           -> AuthMode
           -> m Response
googlePage _here authMode = 
    do u <- showURLParams (A_OpenId (O_Connect authMode)) [("url", google)]
       seeOther u (toResponse ())

yahooPage :: (Happstack m, ShowURL m, URL m ~ AuthURL) => 
             AuthURL
          -> AuthMode
          -> m Response
yahooPage _here authMode =
    do u <- showURLParams (A_OpenId (O_Connect authMode)) [("url", yahoo)]
       seeOther u (toResponse ())
{-
liveJournalPage :: (Happstack m, XMLGenerator m, ToMessage (HSX.XML m), EmbedAsChild m (), Alternative m, ShowURL m, URL m ~ (OpenIdURL p)) =>
                   PageTemplate m
                -> OpenIdURL p
                -> AuthMode
                -> m Response
-}
liveJournalPage appTemplate here authMode =
    do actionURL <- showURL here
       appTemplate "Login" () $
        <div id="main">
         <h1>Login using your Live Journal account</h1>
         <p>Enter your livejournal account name to connect. You may be prompted to log into your livejournal account and to confirm the login.</p>
         <% formPart "p" actionURL handleSuccess (handleFailure appTemplate) liveJournalForm %>
        </div>
      where 
--         handleSuccess :: String -> XMLGenT (RouteT (OpenIdURL p) (ServerPartT IO)) Response
        handleSuccess username =
            do u <- showURLParams (A_OpenId (O_Connect authMode)) [("url", livejournal username)]
               seeOther u (toResponse ())
{-
handleFailure :: (XMLGenerator m, Happstack m, EmbedAsChild m (), ToMessage (HSX.XML m)) =>
                 PageTemplate m
              -> [(FormRange, String)] 
              -> [XMLGenT m (HSX.XML m)]
              -> XMLGenT m Response
-}
handleFailure appTemplate errs formXML =
            XMLGenT $ appTemplate "Login" ()
               <div id="main">
                <h1>Errors</h1>
                <% errorList (map snd errs) %>
                <% formXML %>
               </div>

liveJournalForm :: (Functor v, Monad v, XMLGenerator m) => Form v Input String [XMLGenT m (HSX.XML m)] String
liveJournalForm = 
    label "http://" ++> inputString Nothing <++ label ".livejournal.com/" <* submit "Connect"


handleAuth :: ( Happstack m
              , Alternative m
              , EmbedAsAttr (RouteT AuthURL m) (Attr String AuthURL)
              ) =>
              (forall header body. (EmbedAsChild (RouteT AuthURL m) XML, EmbedAsChild (RouteT AuthURL m) header, EmbedAsChild (RouteT AuthURL m) body) => (String  -> header -> body -> RouteT AuthURL m Response))
     -> Maybe String
     -> String
     -> AuthURL
     -> RouteT AuthURL m Response
handleAuth appTemplate realm onAuthURL url =
    case url of
      A_Login           -> appTemplate "Login"    () loginPage
      A_AddAuth         -> appTemplate "Add Auth" () addAuthPage
      A_Logout          -> appTemplate "Logout"   () logoutPage
      (A_OpenId oidURL) -> nestURL A_OpenId $ handleOpenId realm onAuthURL oidURL
      (A_OpenIdProvider authMode provider) ->  providerPage appTemplate provider url authMode


handleProfile appTemplate url =
    case url of
      P_PickProfile        -> 
          do r <- pickProfile
             case r of
               (Picked {})                -> 
                   seeOther "/" (toResponse "/")
               (PickPersonality profiles) -> 
                   appTemplate "Pick Personality" () (personalityPicker profiles)
               (PickAuthId      authIds)  ->
                   appTemplate "Pick Auth" () (authPicker authIds) 
                  
                              
      (P_SetAuthId authId) -> 
          do b <- setAuthIdPage authId
             if b
              then seeOther "/" (toResponse "") -- FIXME: don't hardcode destination
              else unauthorized =<< 
                     appTemplate "unauthorized" ()
                        <p>Attempted to set AuthId to <% show $ unAuthId authId %>, but failed because the Identifier is not associated with that AuthId.</p>
