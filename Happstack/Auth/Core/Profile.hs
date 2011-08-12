{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeSynonymInstances, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators, RecordWildCards
    #-}

module Happstack.Auth.Core.Profile where

import Control.Applicative 
import Control.Monad.Reader
import Control.Monad.State
import Data.Acid           (AcidState, Update, Query, makeAcidic, query', update')
import Data.Data
import           Data.IxSet (IxSet, (@=), inferIxSet, noCalcs)
import qualified Data.IxSet as IxSet
import Data.Map            (Map)
import qualified Data.Map  as Map
import Data.SafeCopy       (base, deriveSafeCopy)
import Data.Set            (Set)
import qualified Data.Set  as Set
import Data.Text           (Text)
import qualified Data.Text as Text
import Happstack.Auth.Core.Auth
import Happstack.Server
import Web.Routes

newtype UserId = UserId { unUserId :: Integer }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''UserId)

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
$(deriveSafeCopy 1 'base ''Profile)

$(inferIxSet "Profiles" ''Profile 'noCalcs [''UserId, ''AuthId])

data ProfileState 
    = ProfileState { profiles    :: Profiles
                   , authUserMap :: Map AuthId UserId -- ^ map of what 'UserId' an 'AuthId' is currently defaulting to
                   , nextUserId  :: UserId
                   }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''ProfileState)

-- | a reasonable initial 'ProfileState'
initialProfileState :: ProfileState
initialProfileState =
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
    do ps@(ProfileState{..}) <- get
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

$(makeAcidic ''ProfileState 
                [ 'authIdUserId
                , 'authIdProfiles
                , 'setAuthIdUserId
                , 'createNewProfile
                , 'genUserId
                ])

getUserId :: (Alternative m, Happstack m) => AcidState AuthState -> AcidState ProfileState -> m (Maybe UserId)
getUserId authStateH profileStateH =
    do mAuthToken <- getAuthToken  authStateH
       case mAuthToken of
         Nothing -> return Nothing
         (Just authToken) ->
             case tokenAuthId authToken of
               Nothing -> return Nothing
               (Just authId) -> query' profileStateH (AuthIdUserId authId)



