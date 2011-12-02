module Acid where

import Control.Exception           (bracket)
import Data.Acid                   (AcidState, openLocalStateFrom, createCheckpoint, closeAcidState)
import Data.Maybe                  (fromMaybe)
import Happstack.Auth.Core.Auth    (AuthState       , initialAuthState)
import Happstack.Auth.Core.Profile (ProfileState    , initialProfileState)
import ProfileData                 (ProfileDataState, initialProfileDataState)
import System.FilePath             ((</>))

-- | 'Acid' holds all the 'AcidState' handles for this site.
data Acid = Acid
    { acidAuth        :: AcidState AuthState
    , acidProfile     :: AcidState ProfileState
    , acidProfileData :: AcidState ProfileDataState
    }

-- | run an action which takes 'Acid'.
--
-- Uses 'bracket' to open / initialize / close all the 'AcidState' handles. 
--
-- WARNING: The database files should only be opened by one thread in
-- one application at a time. If you want to access the database from
-- multiple threads (which you almost certainly do), then simply pass
-- the 'Acid' handle to each thread.
withAcid :: Maybe FilePath -- ^ state directory
         -> (Acid -> IO a) -- ^ action
         -> IO a
withAcid mBasePath f =
    let basePath = fromMaybe "_state" mBasePath in
    bracket (openLocalStateFrom (basePath </> "auth")        initialAuthState)        (createCheckpointAndClose) $ \auth ->
    bracket (openLocalStateFrom (basePath </> "profile")     initialProfileState)     (createCheckpointAndClose) $ \profile ->
    bracket (openLocalStateFrom (basePath </> "profileData") initialProfileDataState) (createCheckpointAndClose) $ \profileData ->
        f (Acid auth profile profileData)

createCheckpointAndClose st = createCheckpoint st >> closeAcidState st
