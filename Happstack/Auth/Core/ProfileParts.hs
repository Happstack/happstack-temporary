module Happstack.Auth.Core.ProfileParts where

import Control.Applicative (Alternative(..))
import           Data.Set (Set)
import qualified Data.Set as Set
import Happstack.Server
import Happstack.State
import Happstack.Auth.Core.Auth
import Happstack.Auth.Core.ProfileURL
import Happstack.Auth.Core.Profile
import Types ()
import Web.Routes

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

setAuthIdPage :: (Alternative m, Happstack m) => AuthId -> m Bool
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
