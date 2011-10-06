{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Happstack.Auth.Core.AuthProfileURL where

import Control.Applicative ((<$>))
import Control.Monad       (msum)
import Data.Data           (Data, Typeable)
import Happstack.Auth.Core.AuthURL (AuthURL(..))
import Happstack.Auth.Core.ProfileURL (ProfileURL(..))
import Web.Routes          (PathInfo(..), segment)
import Test.QuickCheck     (Arbitrary(..), oneof)

data AuthProfileURL 
    = AuthURL AuthURL
    | ProfileURL ProfileURL
    deriving (Eq, Ord, Read, Show, Data, Typeable)

instance PathInfo AuthProfileURL where
    toPathSegments (AuthURL authURL)       = "auth"    : toPathSegments authURL
    toPathSegments (ProfileURL profileURL) = "profile" : toPathSegments profileURL
    fromPathSegments = 
        msum [ do segment "auth"
                  AuthURL <$> fromPathSegments
             , do segment "profile"
                  ProfileURL <$> fromPathSegments
             ]

instance Arbitrary AuthProfileURL where
    arbitrary = oneof [ AuthURL <$> arbitrary 
                      , ProfileURL <$> arbitrary
                      ]
