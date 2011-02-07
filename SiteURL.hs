{-# LANGUAGE TemplateHaskell #-}
module SiteURL where

import AuthURL
import Web.Routes
import Web.Routes.TH

data SiteURL 
    = U_HomePage
    | U_Auth AuthURL
    deriving (Eq, Ord, Read, Show)

$(derivePathInfo ''SiteURL)

