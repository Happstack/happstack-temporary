module Happstack.Auth.Core.ProfileParts where

import Control.Applicative (Alternative(..))
import           Data.Acid (AcidState, update', query')
import           Data.Set  (Set)
import qualified Data.Set  as Set
import Happstack.Server
import Happstack.Auth.Core.Auth
import Happstack.Auth.Core.ProfileURL
import Happstack.Auth.Core.Profile
import Web.Routes
import Web.Routes.Happstack

-- * ProfileURL stuff

-- can we pick an AuthId with only the information in the Auth stuff? Or should that be a profile action ?

pickAuthId :: (Happstack m, Alternative m) => AcidState AuthState -> m (Either (Set AuthId) AuthId)
pickAuthId authStateH =
    do (Just authToken) <- getAuthToken authStateH -- FIXME: Just 
       case tokenAuthId authToken of
         (Just authId) -> return (Right authId)
         Nothing ->
             do authIds <- case tokenAuthMethod authToken of
                             (AuthIdentifier identifier) -> query' authStateH (IdentifierAuthIds identifier)
                             (AuthFacebook   facebookId) -> query' authStateH (FacebookAuthIds facebookId)
                case Set.size authIds of
                  0 -> do authId <- update' authStateH (NewAuthMethod (tokenAuthMethod authToken))
                          update' authStateH (UpdateAuthToken (authToken { tokenAuthId = Just authId }))
                          return (Right authId)
                  1 -> do let aid = head $ Set.toList authIds
                          update' authStateH (UpdateAuthToken (authToken { tokenAuthId = Just aid }))
                          return (Right aid)
                  n -> return (Left authIds)

setAuthIdPage :: (Alternative m, Happstack m) => AcidState AuthState -> AuthId -> m Bool
setAuthIdPage authStateH authId =
    do mAuthToken <- getAuthToken authStateH
       case mAuthToken of
         Nothing -> undefined -- FIXME
         (Just authToken) ->
             do authIds <- case tokenAuthMethod authToken of
                             (AuthIdentifier identifier) -> query' authStateH (IdentifierAuthIds identifier)
                             (AuthFacebook   facebookId) -> query' authStateH (FacebookAuthIds facebookId)
                if Set.member authId authIds
                   then do update' authStateH (UpdateAuthToken (authToken { tokenAuthId = Just authId }))
                           return True
                   else return False

data PickProfile 
    = Picked UserId
    | PickPersonality (Set Profile)
    | PickAuthId      (Set AuthId)

pickProfile :: (Happstack m, Alternative m) => AcidState AuthState -> AcidState ProfileState -> m PickProfile
pickProfile authStateH profileStateH =
    do eAid <- pickAuthId authStateH
       case eAid of
         (Right aid) ->
             do mUid <- query' profileStateH (AuthIdUserId aid)
                case mUid of
                  Nothing ->
                      do profiles <- query' profileStateH (AuthIdProfiles aid)
                         case Set.size profiles of
                           0 -> do uid <- update' profileStateH (CreateNewProfile (Set.singleton aid))
                                   update' profileStateH (SetAuthIdUserId aid uid)
                                   return (Picked uid)
--                                   seeOther onLoginURL (toResponse onLoginURL)
                           1 -> do let profile = head $ Set.toList profiles
                                   update' profileStateH (SetAuthIdUserId aid (userId profile))
                                   return (Picked (userId profile))
                           n -> do return (PickPersonality profiles)
                  (Just uid) ->
                      return (Picked uid)
         (Left aids) -> return (PickAuthId aids)
