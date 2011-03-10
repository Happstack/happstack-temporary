{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeSynonymInstances, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators, RecordWildCards
    #-}

module Happstack.Auth.Core.Profile where

import Control.Applicative 
import Control.Monad.Reader
import Control.Monad.State
import Data.Data
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Happstack.Auth.Core.Auth
import Happstack.Data
import Happstack.State
import Happstack.Server
import           Happstack.Data.IxSet (IxSet, (@=), inferIxSet, noCalcs)
import qualified Happstack.Data.IxSet as IxSet
import Web.Routes

newtype UserId = UserId { unUserId :: Integer }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
instance Version UserId
$(deriveSerialize ''UserId)
$(deriveNewData [''UserId])

instance PathInfo UserId where
    toPathSegments (UserId i) = toPathSegments i
    fromPathSegments = UserId <$> fromPathSegments

succUserId :: UserId -> UserId
succUserId (UserId i) = UserId (succ i)


data Profile 
    = Profile
    { userId :: UserId
    , auths  :: Set AuthId
    , nickName :: Text
    }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
instance Version Profile
$(deriveSerialize ''Profile)
$(deriveNewData [''Profile])

$(inferIxSet "Profiles" ''Profile 'noCalcs [''UserId, ''AuthId])

data ProfileState 
    = ProfileState { profiles    :: Profiles
                   , authUserMap :: Map AuthId UserId -- ^ map of what 'UserId' an 'AuthId' is currently defaulting to
                   , nextUserId  :: UserId
                   }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
instance Version ProfileState
$(deriveSerialize ''ProfileState)
$(deriveNewData [''ProfileState])

instance Component ProfileState where
    type Dependencies ProfileState = End
    initialValue =
        ProfileState { profiles    = IxSet.empty
                     , authUserMap = Map.empty
                     , nextUserId  = UserId 1
                     }

genUserId :: Update ProfileState UserId
genUserId =
    do as@(ProfileState {..}) <- get
       put (as { nextUserId = succUserId nextUserId })
       return nextUserId

-- return the UserId currently prefered by this AuthId
--
-- can be Nothing if no preference is set, even if there are possible UserIds
authIdUserId :: AuthId -> Query ProfileState (Maybe UserId)
authIdUserId aid =
    do ps@(ProfileState {..}) <- ask
       return $ Map.lookup aid authUserMap 

-- return all the Profiles associated with this AuthId
authIdProfiles :: AuthId -> Query ProfileState (Set Profile)
authIdProfiles aid =
    do ps@(ProfileState {..}) <- ask
       return $ IxSet.toSet (profiles @= aid)

setAuthIdUserId :: AuthId -> UserId -> Update ProfileState ()
setAuthIdUserId authId userId =
    do ps@(ProfileState{..}) <- ask
       put $ ps { authUserMap = Map.insert authId userId authUserMap }

createNewProfile :: Set AuthId -> Update ProfileState UserId
createNewProfile authIds =
    do ps@(ProfileState{..}) <- get
       let profile = Profile { userId = nextUserId
                             , auths  = authIds
                             , nickName = Text.pack "Anonymous"
                             }
       put $ (ps { profiles    = IxSet.insert profile profiles 
                 , nextUserId  = succUserId nextUserId
                 })
       return nextUserId

$(mkMethods ''ProfileState 
                [ 'authIdUserId
                , 'authIdProfiles
                , 'setAuthIdUserId
                , 'createNewProfile
                , 'genUserId
                ])

getUserId :: (Alternative m, Happstack m) => m (Maybe UserId)
getUserId =
    do mTokenStr <- optional $ lookCookieValue "authToken"
       case mTokenStr of
         Nothing -> return Nothing
         (Just tokenStr) ->
             do mAuthId <- query (AuthTokenAuthId tokenStr)
                case mAuthId of
                  Nothing -> return Nothing
                  (Just authId) ->
                      query (AuthIdUserId authId)



