{-# LANGUAGE TypeFamilies #-}
module Happstack.Auth.Core.AuthParts where

import Control.Applicative
import Control.Monad.Trans
import Data.Acid                  (AcidState, query', update')
import Data.Maybe                 (mapMaybe)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import Happstack.Server
import Happstack.Auth.Core.Auth
import Happstack.Auth.Core.AuthURL
import Web.Authenticate.OpenId    (Identifier, authenticate, getForwardUrl)
import Web.Routes

-- this verifies the identifier
-- and sets authToken cookie
-- if the identifier was not associated with an AuthId, then a new AuthId will be created and associated with it.
openIdPage :: (Alternative m, Happstack m) =>
              AcidState AuthState 
           -> AuthMode 
           -> String 
           -> m Response
openIdPage acid LoginMode onAuthURL = 
    do identifier <- getIdentifier
       addAuthIdsCookie acid identifier
       seeOther onAuthURL (toResponse ())
openIdPage acid AddIdentifierMode onAuthURL =
    do identifier <- getIdentifier
       mAuthId    <- getAuthId acid
       case mAuthId of
         Nothing       -> undefined -- FIXME
         (Just authId) ->
             do update' acid (AddAuthMethod (AuthIdentifier identifier) authId)
                seeOther onAuthURL (toResponse ())

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
addAuthIdsCookie :: (Happstack m) => AcidState AuthState -> Identifier -> m (Maybe AuthId)
addAuthIdsCookie acid identifier =
    do authId <- 
           do authIds <- query' acid (IdentifierAuthIds identifier)
              case Set.size authIds of
                1 -> return $ (Just $ head $ Set.toList $ authIds)
                n -> return $ Nothing
       addAuthCookie acid authId (AuthIdentifier identifier)
       return authId

connect :: (Happstack m, ShowURL m, URL m ~ OpenIdURL) => 
              AuthMode     -- ^ authentication mode
           -> Maybe String -- ^ realm
           -> String       -- ^ openid url
           -> m Response
connect authMode realm url = 
    do openIdUrl <- showURL (O_OpenId authMode)
       gotoURL <- liftIO $ getForwardUrl (T.pack url) (T.pack openIdUrl) (T.pack <$> realm) []
       seeOther gotoURL (toResponse gotoURL)

-- type ProviderPage m p = (OpenIdURL p) -> AuthMode -> m Response

handleOpenId :: (Alternative m, Happstack m, ShowURL m, URL m ~ OpenIdURL) =>
                AcidState AuthState
             -> Maybe String -- ^ realm
             -> String       -- ^ onAuthURL
             -> OpenIdURL    -- ^ this url
             -> m Response
handleOpenId acid realm onAuthURL url =
    case url of
      (O_OpenId authMode)                  -> openIdPage acid authMode onAuthURL
      (O_Connect authMode)                 -> 
          do url <- look "url"
             connect authMode realm url
