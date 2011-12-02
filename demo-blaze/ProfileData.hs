{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, TemplateHaskell, MultiParamTypeClasses, RecordWildCards, TypeFamilies #-}
-- | The 'ProfileData' module contains application specific data
-- linked to a specific 'UserId'. This module is in no way required by
-- the happstack-authenticate library. In general, people do want to
-- store some sort of profile data, such as the user's name, email
-- address, etc.
--
-- 'happstack-authentication' does not provide any built-in mechanism
-- for storing that data. Mostly because there is no point. This
-- module shows how easy it is to create your own mechanism for
-- storing profile data that does exactly what you want.
--
-- If you don't use acid-state for the rest of your application, then
-- you could just as easily store the data in SQL, etc.
module ProfileData where

import Control.Monad.Reader  (ask)
import Control.Monad.State   (get, put)
import Control.Monad.Trans   (liftIO)
import Data.Acid             (AcidState, Update, Query, makeAcidic)
import Data.Acid.Advanced    (update')
import Data.Generics         (Data, Typeable)
import           Data.IxSet  ((@=), getOne, inferIxSet, noCalcs)
import qualified Data.IxSet  as IxSet
import Data.SafeCopy         (base, deriveSafeCopy)
import Data.Text             (Text)
import qualified Data.Text   as Text
import Happstack.Auth        (AuthState, ProfileState, UserId, getUserId)  
import Happstack.Server      (Happstack, Response, ServerPartT, dir, internalServerError, ok, seeOther, toResponse)

-- | 'ProfileData' contains application specific 
data ProfileData = 
    ProfileData { dataFor    :: UserId -- ^ UserId associated with this profile data
                , profileMsg :: Text   -- ^ Some data to store in the profile
                }
    deriving (Eq, Ord, Read, Show, Typeable, Data)
$(deriveSafeCopy 1 'base ''ProfileData)

$(inferIxSet "ProfilesData" ''ProfileData 'noCalcs [''UserId, ''Text])

data ProfileDataState =
    ProfileDataState { profilesData :: ProfilesData }
    deriving (Eq, Ord, Read, Show, Typeable, Data)
$(deriveSafeCopy 1 'base ''ProfileDataState)

-- | set 'ProfileData' for UserId
setProfileData :: UserId -> Text -> Update ProfileDataState ProfileData
setProfileData uid msg =
    do pds@(ProfileDataState {..}) <- get       
       let profileData = ProfileData uid msg
       put $ pds { profilesData = IxSet.updateIx uid profileData profilesData }
       return profileData

-- | get 'ProfileData' associated with 'UserId'
askProfileData :: UserId -> Query ProfileDataState (Maybe ProfileData)
askProfileData uid =
    do ProfileDataState{..} <- ask
       return $ getOne $ profilesData @= uid

-- | create the profile data, but only if it is missing
newProfileData :: UserId -> Text -> Update ProfileDataState ProfileData
newProfileData uid msg = 
    do pds@(ProfileDataState {..}) <- get       
       case IxSet.getOne (profilesData @= uid) of
         Nothing -> do let profileData = ProfileData uid msg
                       put $ pds { profilesData = IxSet.updateIx uid profileData profilesData }
                       return profileData
         (Just profileData) -> return profileData

$(makeAcidic ''ProfileDataState 
                [ 'setProfileData
                , 'askProfileData
                , 'newProfileData
                ]
 )
 
initialProfileDataState :: ProfileDataState
initialProfileDataState = ProfileDataState { profilesData = IxSet.empty }

handleProfileData :: AcidState AuthState
                  -> AcidState ProfileState
                  -> AcidState ProfileDataState
                  -> ServerPartT IO Response
handleProfileData authStateH profileStateH profileDataStateH=
    dir "profile_data" $ 
      dir "new" $
          do mUserId <- getUserId authStateH profileStateH
             case mUserId of
               Nothing -> internalServerError $ toResponse $ "not logged in."
               (Just userId) ->
                   do update' profileDataStateH (NewProfileData userId (Text.pack "this is the default message."))
                      seeOther "/" (toResponse "/")
