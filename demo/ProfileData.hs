{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, TemplateHaskell, MultiParamTypeClasses, RecordWildCards, TypeFamilies #-}
module ProfileData where

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.Generics
import Data.SafeCopy
import Happstack.Auth.Core.Profile
import Happstack.Data.IxSet        (IxSet, (@=), getOne, inferIxSet, noCalcs)
import Happstack.Server
import qualified Happstack.Data.IxSet  as IxSet
import Data.Text                   (Text)
import qualified Data.Text         as Text
import Web.Routes.TH

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
