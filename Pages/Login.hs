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
import Pages.AppTemplate
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


-- this verifies the identifier
-- and sets authToken cookie
-- if the identifier was not associated with an AuthId, then a new AuthId will be created and associated with it.
openIdPage :: AuthMode -> String -> RouteT AuthURL (ServerPartT IO) Response -- (Maybe AuthId)
openIdPage LoginMode onAuthURL = 
    do identifier <- getIdentifier
       addAuthIdsCookie identifier
       seeOther onAuthURL (toResponse ())
openIdPage AddIdentifierMode onAuthURL =
    do identifier <- getIdentifier
       mAuthId <- getAuthId
       case mAuthId of
         Nothing       -> undefined
         (Just authId) ->
             do update (AddAuthMethod (AuthIdentifier identifier) authId)
                seeOther onAuthURL (toResponse ())

-- this get's the identifier the openid provider provides. It is our only chance to capture the Identifier. So, before we send a Response we need to have some sort of cookie set that identifies the user. We can not just put the identifier in the cookie because we don't want some one to fake it.
getIdentifier :: (Happstack m) => m Identifier
getIdentifier =
    do pairs'      <- lookPairs
       let pairs = mapMaybe (\(k, ev) -> case ev of (Left _) -> Nothing ; (Right v) -> Just (k, v)) pairs'
       (identifier, _) <- liftIO $ authenticate pairs
       return identifier

-- calling this will log you in as 1 or more AuthIds
-- problem.. if the Identifier is not associated with any Auths, then we are in trouble, because the identifier will be 'lost'.
-- so, if there are no AuthIds associated with the identifier, we create one.
--
-- we have another problem though.. we want to allow a user to specify a prefered AuthId. But that preference needs to be linked to a specific Identifier ?
addAuthIdsCookie :: (Happstack m) => Identifier -> m (Maybe AuthId)
addAuthIdsCookie identifier =
    do authId <- 
           do authIds <- query (IdentifierAuthIds identifier)
              case Set.size authIds of
                 1 -> return $ (Just $ head $ Set.toList $ authIds)
                 n -> return $ Nothing
       addAuthCookie authId (AuthIdentifier identifier)
       return authId

-- * ProfileURL stuff

-- can we pick an AuthId with only the information in the Auth stuff? Or should that be a profile action ?

pickAuthId :: RouteT ProfileURL (ServerPartT IO) (Either (Set AuthId) AuthId)
pickAuthId =
    do (Just authToken) <- getAuthToken -- FIXME: Just 
       case tokenAuthId authToken of
         (Just authId) -> return (Right authId)
         Nothing ->
             do authIds <- query (IdentifierAuthIds (amIdentifier $ tokenAuthMethod authToken)) -- FIXME: might not be an Identifier
                case Set.size authIds of
                  0 -> do authId <- update (NewAuthMethod (tokenAuthMethod authToken))
                          update (UpdateAuthToken (authToken { tokenAuthId = Just authId }))
                          return (Right authId)
                  1 -> do let aid = head $ Set.toList authIds
                          update (UpdateAuthToken (authToken { tokenAuthId = Just aid }))
                          return (Right aid)
                  n -> return (Left authIds)

setAuthIdPage :: AuthId -> RouteT ProfileURL (ServerPartT IO) Bool
setAuthIdPage authId =
    do mAuthToken <- getAuthToken
       case mAuthToken of
         Nothing -> undefined
         (Just authToken) ->
             do authIds <- query (IdentifierAuthIds (amIdentifier $ tokenAuthMethod authToken)) -- FIXME: might not be an Identifier
                if Set.member authId authIds
                   then do update (UpdateAuthToken (authToken { tokenAuthId = Just authId }))
                           return True
                   else return False

data PickProfile 
    = Picked UserId
    | PickPersonality (Set Profile)
    | PickAuthId      (Set AuthId)

pickProfile :: RouteT ProfileURL (ServerPartT IO) PickProfile
pickProfile =
    do eAid <- pickAuthId
       case eAid of
         (Right aid) ->
             do mUid <- query (AuthIdUserId aid)
                case mUid of
                  Nothing ->
                      do profiles <- query (AuthIdProfiles aid)
                         case Set.size profiles of
                           0 -> do uid <- update (CreateNewProfile (Set.singleton aid))
                                   update (SetAuthIdUserId aid uid)
                                   return (Picked uid)
--                                   seeOther onLoginURL (toResponse onLoginURL)
                           1 -> do let profile = head $ Set.toList profiles
                                   update (SetAuthIdUserId aid (userId profile))
                                   return (Picked (userId profile))
                           n -> do return (PickPersonality profiles)
                  (Just uid) ->
                      return (Picked uid)
         (Left aids) -> return (PickAuthId aids)
