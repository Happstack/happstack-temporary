{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, TemplateHaskell, MultiParamTypeClasses, RecordWildCards, TypeFamilies #-}
module ProfileData where

import Control.Monad.Reader
import Control.Monad.State
import Data.Generics
import Happstack.State
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

instance Version ProfileData
$(deriveSerialize ''ProfileData)

$(inferIxSet "ProfilesData" ''ProfileData 'noCalcs [''UserId, ''Text])

data ProfileDataState =
    ProfileDataState { profilesData :: ProfilesData }
    deriving (Eq, Ord, Read, Show, Typeable, Data)
instance Version ProfileDataState
$(deriveSerialize ''ProfileDataState)

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

$(mkMethods ''ProfileDataState 
                [ 'newProfileData
                , 'askProfileData
                ]
 )

instance Component ProfileDataState where
    type Dependencies ProfileDataState = End
    initialValue = ProfileDataState { profilesData = IxSet.empty }

data ProfileDataURL 
    = CreateNewProfileData 
    | ViewProfileData UserId
      deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePathInfo ''ProfileDataURL)

handleProfileData url =
    case url of
      CreateNewProfileData ->
          do mUserId <- getUserId
             case mUserId of
               Nothing -> internalServerError $ toResponse $ "not logged in."
               (Just userId) ->
                   do update (NewProfileData userId (Text.pack "this is the default message."))
                      seeOther "/" (toResponse "/")
      (ViewProfileData uid) ->
          do mProfileData <- query (AskProfileData uid)
             ok $ toResponse $ show mProfileData
