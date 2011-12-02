{-# LANGUAGE TypeFamilies, ViewPatterns, RecordWildCards #-}
module Happstack.Auth.Core.AuthParts where

import Control.Applicative
import Control.Monad.Trans
import Data.Acid                  (AcidState)
import Data.Acid.Advanced         (query', update')
import Data.Aeson                 (Value(..))
import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe                 (mapMaybe)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import qualified Data.Text               as T
import           Data.Text               (Text)
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import Happstack.Server
import Happstack.Auth.Core.Auth   
import Happstack.Auth.Core.AuthURL
import Web.Authenticate.OpenId    (Identifier, authenticate, getForwardUrl)
import Web.Authenticate.Facebook  (Facebook(..), getAccessToken, getGraphData)
import qualified Web.Authenticate.Facebook as Facebook
import Web.Routes

-- this verifies the identifier
-- and sets authToken cookie
-- if the identifier was not associated with an AuthId, then a new AuthId will be created and associated with it.
openIdPage :: (Alternative m, Happstack m) =>
              AcidState AuthState 
           -> AuthMode 
           -> Text
           -> m Response
openIdPage acid LoginMode onAuthURL = 
    do identifier <- getIdentifier
       identifierAddAuthIdsCookie acid identifier
       seeOther (T.unpack onAuthURL) (toResponse ())
openIdPage acid AddIdentifierMode onAuthURL =
    do identifier <- getIdentifier
       mAuthId    <- getAuthId acid
       case mAuthId of
         Nothing       -> undefined -- FIXME
         (Just authId) ->
             do update' acid (AddAuthMethod (AuthIdentifier identifier) authId)
                seeOther (T.unpack onAuthURL) (toResponse ())

-- this get's the identifier the openid provider provides. It is our only chance to capture the Identifier. So, before we send a Response we need to have some sort of cookie set that identifies the user. We can not just put the identifier in the cookie because we don't want some one to fake it.
getIdentifier :: (Happstack m) => m Identifier
getIdentifier =
    do pairs'      <- lookPairsBS
       let pairs = mapMaybe (\(k, ev) -> case ev of (Left _) -> Nothing ; (Right v) -> Just (T.pack k, TL.toStrict $ TL.decodeUtf8 v)) pairs'
       (identifier, _) <- liftIO $ authenticate pairs
       return identifier

-- calling this will log you in as 1 or more AuthIds
-- problem.. if the Identifier is not associated with any Auths, then we are in trouble, because the identifier will be 'lost'.
-- so, if there are no AuthIds associated with the identifier, we create one.
--
-- we have another problem though.. we want to allow a user to specify a prefered AuthId. But that preference needs to be linked to a specific Identifier ?
identifierAddAuthIdsCookie :: (Happstack m) => AcidState AuthState -> Identifier -> m (Maybe AuthId)
identifierAddAuthIdsCookie acid identifier =
    do authId <- 
           do authIds <- query' acid (IdentifierAuthIds identifier)
              case Set.size authIds of
                1 -> return $ (Just $ head $ Set.toList $ authIds)
                n -> return $ Nothing
       addAuthCookie acid authId (AuthIdentifier identifier)
       return authId

facebookAddAuthIdsCookie :: (Happstack m) => AcidState AuthState -> FacebookId -> m (Maybe AuthId)
facebookAddAuthIdsCookie acid facebookId =
    do authId <- 
           do authIds <- query' acid (FacebookAuthIds facebookId)
              case Set.size authIds of
                1 -> return $ (Just $ head $ Set.toList $ authIds)
                n -> return $ Nothing
       addAuthCookie acid authId (AuthFacebook facebookId)
       return authId


connect :: (Happstack m, MonadRoute m, URL m ~ OpenIdURL) => 
              AuthMode     -- ^ authentication mode
           -> Maybe Text -- ^ realm
           -> Text       -- ^ openid url
           -> m Response
connect authMode realm url = 
    do openIdUrl <- showURL (O_OpenId authMode)
       gotoURL <- liftIO $ getForwardUrl url openIdUrl realm []
       seeOther (T.unpack gotoURL) (toResponse gotoURL)

-- type ProviderPage m p = (OpenIdURL p) -> AuthMode -> m Response

handleOpenId :: (Alternative m, Happstack m, MonadRoute m, URL m ~ OpenIdURL) =>
                AcidState AuthState
             -> Maybe Text   -- ^ realm
             -> Text         -- ^ onAuthURL
             -> OpenIdURL    -- ^ this url
             -> m Response
handleOpenId acid realm onAuthURL url =
    case url of
      (O_OpenId authMode)                  -> openIdPage acid authMode onAuthURL
      (O_Connect authMode)                 -> 
          do url <- lookText "url"
             connect authMode realm (TL.toStrict url)

makeFacebook :: (Monad m, URL m ~ AuthURL, MonadRoute m) => 
                Text 
             -> Text
             -> AuthMode
             -> m Facebook
makeFacebook clientId clientSecret authMode =
    do redirectUri <- showURL (A_FacebookRedirect authMode)
       return (Facebook { facebookClientId     = clientId
                        , facebookClientSecret = clientSecret
                        , facebookRedirectUri  = redirectUri 
                        })

facebookPage :: (Happstack m, MonadRoute m, URL m ~ AuthURL) => Facebook -> AuthMode -> m Response
facebookPage Facebook{..} authMode = 
    do facebook <- makeFacebook facebookClientId facebookClientSecret authMode
       let url = Facebook.getForwardUrl facebook []
       seeOther (T.unpack url) (toResponse ())

facebookRedirectPage :: (Happstack m, MonadRoute m, URL m ~ AuthURL) =>
                        AcidState AuthState
                     -> Facebook
                     -> Text -- ^ onAuthURL
                     -> AuthMode
                     -> m Response
facebookRedirectPage acidAuth Facebook{..} onAuthURL authMode =
    do facebook <- makeFacebook facebookClientId facebookClientSecret authMode
       code  <- lookText "code"
       token <- liftIO $ getAccessToken facebook (TL.toStrict code)
       gd    <- liftIO $ getGraphData token (T.pack "me")
       case gd of
         (Right (Object (HashMap.lookup (T.pack "id") -> (Just (String facebookId))))) ->
             case authMode of
               LoginMode ->
                   do facebookAddAuthIdsCookie acidAuth (FacebookId facebookId)
                      seeOther (T.unpack onAuthURL) (toResponse ())
               AddIdentifierMode ->
                   do mAuthId <- getAuthId acidAuth
                      case mAuthId of
                        Nothing       -> internalServerError $ toResponse $ "Could not add new authentication method because the user is not logged in."
                        (Just authId) ->
                            do update' acidAuth (AddAuthMethod (AuthFacebook (FacebookId facebookId)) authId)
                               seeOther (T.unpack onAuthURL) (toResponse ())
         _ -> internalServerError $ toResponse $ "failed to extract user id: " ++ show gd