{-# LANGUAGE TemplateHaskell #-}
module SiteURL where

import Happstack.Auth (AuthProfileURL)
import ProfileData    (ProfileDataURL)
import Web.Routes.TH  (derivePathInfo)

data SiteURL 
    = U_HomePage
    | U_AuthProfile AuthProfileURL
    | U_ProfileData ProfileDataURL
    deriving (Eq, Ord, Read, Show)

$(derivePathInfo ''SiteURL)

