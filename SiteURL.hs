{-# LANGUAGE TemplateHaskell #-}
module SiteURL where

import AuthURL
import ProfileURL
import Web.Routes
import Web.Routes.TH

data SiteURL 
    = U_HomePage
    | U_Auth AuthURL
    | U_Profile ProfileURL
    deriving (Eq, Ord, Read, Show)

$(derivePathInfo ''SiteURL)

