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

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.Generics
import           Data.IxSet  (IxSet, (@=), getOne, inferIxSet, noCalcs)
import qualified Data.IxSet  as IxSet
import Data.SafeCopy
import Data.Text                   (Text)
import qualified Data.Text         as Text
import Happstack.Auth.Core.Profile
import Happstack.Server
import Web.Routes.TH

-- | 'ProfileData' contains application specific 
data ProfileData = 
    ProfileData { dataFor :: UserId
                , profileMsg :: Text
                }
    deriving (Eq, Ord, Read, Show, Typeable, Data)
$(deriveSafeCopy 1 'base ''ProfileData)

$(inferIxSet "ProfilesData" ''ProfileData 'noCalcs [''UserId, ''Text])

data ProfileDataState =
    ProfileDataState { profilesData :: ProfilesData }
    deriving (Eq, Ord, Read, Show, Typeable, Data)
$(deriveSafeCopy 1 'base ''ProfileDataState)

-- FIXME: this should be idempotent
newProfileData :: UserId -> Text -> Update ProfileDataState ProfileData
newProfileData uid msg =
    do pds@(ProfileDataState {..}) <- get       
       let profileData = ProfileData uid msg
       put $ pds { profilesData = IxSet.insert profileData profilesData }
       return profileData

askProfileData :: UserId -> Query ProfileDataState (Maybe ProfileData)
askProfileData uid =
    do ProfileDataState{..} <- ask
       return $ getOne $ profilesData @= uid

$(makeAcidic ''ProfileDataState 
                [ 'newProfileData
                , 'askProfileData
                ]
 )
 
initialProfileDataState :: ProfileDataState
initialProfileDataState = ProfileDataState { profilesData = IxSet.empty }

data ProfileDataURL 
    = CreateNewProfileData 
    | ViewProfileData UserId
      deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePathInfo ''ProfileDataURL)

handleProfileData authStateH profileStateH profileDataStateH url =
    case url of
      CreateNewProfileData ->
          do mUserId <- getUserId authStateH profileStateH
             case mUserId of
               Nothing -> internalServerError $ toResponse $ "not logged in."
               (Just userId) ->
                   do update' profileDataStateH (NewProfileData userId (Text.pack "this is the default message."))
                      seeOther "/" (toResponse "/")
      (ViewProfileData uid) ->
          do mProfileData <- query' profileDataStateH (AskProfileData uid)
             ok $ toResponse $ show mProfileData
