{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Pages.Login where

import AuthURL
import Control.Applicative        (Alternative, (<*>), (<$>), (<*), optional)
import Control.Monad              (replicateM, mplus)
import Control.Monad.Trans        (MonadIO(liftIO))
import Data.Maybe                 (mapMaybe)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import Data.Text                  (Text)
import Data.Time.Clock            (getCurrentTime)
import Happstack.Server           -- (CookieLife(Session), Response, ServerMonad(..), FilterMonad(..), Input(..), Happstack, ServerPartT, addCookie, escape, internalServerError, lookCookieValue, lookPairs, mkCookie, seeOther, toResponse, unauthorized)
import Happstack.Server.HSP.HTML  (XML)
import Happstack.State            (query, update)
import HSP                        (Attr(..), EmbedAsAttr(..), EmbedAsChild(..), XMLGenT, XMLGenerator, genElement, unXMLGenT)
import HSP.ServerPartT()
import qualified HSX.XMLGenerator as HSX
import State.Auth
import Pages.Auth
import Pages.AppTemplate
import Pages.Profile
import Pages.FormPart
import Profile
import ProfileURL
import Text.Digestive
import Text.Digestive.Forms.Happstack ()
import Text.Digestive.HSP.Html4
import Types (UserId(..))
import Web.Authenticate.OpenId    (Identifier, authenticate, getForwardUrl)
import Web.Authenticate.OpenId.Providers (google, yahoo, livejournal, myspace)
import Web.Routes                 (RouteT, ShowURL, showURL, showURLParams, URL)
import Web.Routes.XMLGenT

-- * AuthURL stuff

loginPage :: XMLGenT (RouteT AuthURL (ServerPartT IO)) XML
loginPage =
      <ol>
       <li><a href=(A_OpenId (O_OpenIdProvider LoginMode Google))     >Login</a> with your Google</li>
       <li><a href=(A_OpenId (O_OpenIdProvider LoginMode Yahoo))      >Login</a> with your Yahoo Account</li>
       <li><a href=(A_OpenId (O_OpenIdProvider LoginMode LiveJournal))>Login</a> with your Live Journal Account</li>
       <li><a href=(A_OpenId (O_OpenIdProvider LoginMode Myspace))    >Login</a> with your Myspace Account</li>
       <li><a href=(A_OpenId (O_OpenIdProvider LoginMode Generic))    >Login</a> with your OpenId Account</li>
      </ol>

addAuthPage :: XMLGenT (RouteT AuthURL (ServerPartT IO)) XML
addAuthPage =
      <ol>
       <li><a href=(A_OpenId (O_OpenIdProvider AddIdentifierMode Google))     >Add</a> your Google</li>
       <li><a href=(A_OpenId (O_OpenIdProvider AddIdentifierMode Yahoo))      >Add</a> your Yahoo Account</li>
       <li><a href=(A_OpenId (O_OpenIdProvider AddIdentifierMode LiveJournal))>Add</a> your Live Journal Account</li>
       <li><a href=(A_OpenId (O_OpenIdProvider AddIdentifierMode Myspace))    >Add</a> your Myspace Account</li>
       <li><a href=(A_OpenId (O_OpenIdProvider AddIdentifierMode Generic))    >Add</a> your OpenId Account</li>
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

type ProviderPage p = (OpenIdURL p) -> AuthMode -> RouteT (OpenIdURL p) (ServerPartT IO) Response

providerPage :: OpenIdProvider -> ProviderPage p
providerPage Google      = googlePage
providerPage Yahoo       = yahooPage
providerPage LiveJournal = liveJournalPage

googlePage :: (Happstack m, ShowURL m, URL m ~ (OpenIdURL p)) =>
              (OpenIdURL p)
           -> AuthMode
           -> m Response
googlePage _here authMode = 
    do u <- showURLParams (O_Connect authMode) [("url", google)]
       seeOther u (toResponse ())

yahooPage :: (Happstack m, ShowURL m, URL m ~ (OpenIdURL p)) => 
             (OpenIdURL p)
          -> AuthMode
          -> m Response
yahooPage _here authMode =
    do u <- showURLParams (O_Connect authMode) [("url", yahoo)]
       seeOther u (toResponse ())

liveJournalPage :: OpenIdURL p
                -> AuthMode
                -> RouteT (OpenIdURL p) (ServerPartT IO) Response
liveJournalPage here authMode =
    do actionURL <- showURL here
       appTemplate "Login" () $
        <div id="main">
         <h1>Login using your Live Journal account</h1>
         <p>Enter your livejournal account name to connect. You may be prompted to log into your livejournal account and to confirm the login.</p>
         <% formPart "p" actionURL handleSuccess handleFailure usernameForm %>
        </div>
      where 
        usernameForm = 
            label "http://" ++> inputString Nothing <++ label ".livejournal.com/" <* submit "Connect"
        handleSuccess :: String -> XMLGenT (RouteT (OpenIdURL p) (ServerPartT IO)) Response
        handleSuccess username =
            do u <- showURLParams (O_Connect authMode) [("url", livejournal username)]
               seeOther u (toResponse ())

handleFailure :: [(FormRange, String)] 
              -> [XMLGenT (RouteT (OpenIdURL p) (ServerPartT IO)) (HSX.XML (RouteT (OpenIdURL p) (ServerPartT IO)))] 
              -> XMLGenT (RouteT (OpenIdURL p) (ServerPartT IO)) Response
handleFailure errs formXML =
            toResponse <$> appTemplate' "Login" ()
               <div id="main">
                <h1>Errors</h1>
                <% errorList (map snd errs) %>
                <% formXML %>
               </div>

liveJournalForm :: (Functor v, Monad v, XMLGenerator m) => Form v Input e [XMLGenT m (HSX.XML m)] String
liveJournalForm = 
    label "http://" ++> inputString Nothing <++ label ".livejournal.com/" <* submit "Connect"


