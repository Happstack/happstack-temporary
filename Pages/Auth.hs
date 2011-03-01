module Pages.Auth where

import AuthURL
import Control.Monad.Trans
import Data.Maybe                 (mapMaybe)
import           Data.Set         (Set)
import qualified Data.Set as Set
import Happstack.Server
import Happstack.State
import State.Auth
import Types
import Web.Authenticate.OpenId    (Identifier, authenticate, getForwardUrl)
import Web.Routes

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

