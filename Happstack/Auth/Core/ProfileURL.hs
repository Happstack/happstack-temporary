{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances #-}
module Happstack.Auth.Core.ProfileURL where

import Control.Applicative ((<$>))
import Control.Monad
import Data.Data
import Data.Typeable
import Happstack.Server
import Web.Routes.Happstack
import Web.Routes.XMLGenT
import Happstack.Auth.Core.Auth
import Happstack.Auth.Core.Profile
import Test.QuickCheck

import Web.Routes
data ProfileURL
    = P_SetPersonality UserId
    | P_SetAuthId      AuthId
    | P_PickProfile
      deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Arbitrary ProfileURL where
    arbitrary = oneof $ [ P_SetPersonality . UserId <$> arbitrary
                        , P_SetAuthId      . AuthId <$> arbitrary
                        , return P_PickProfile
                        ]


instance PathInfo ProfileURL where
    toPathSegments (P_SetPersonality userId) = "set_personality" : toPathSegments userId
    toPathSegments (P_SetAuthId authId)      = "set_authid"      : toPathSegments authId
    toPathSegments P_PickProfile             = ["pick_profile"]

    fromPathSegments =
        msum [ do segment "set_personality"
                  userId <- fromPathSegments
                  return (P_SetPersonality userId)
             , do segment "set_authid"
                  authId <- fromPathSegments
                  return (P_SetAuthId authId)
             , do segment "pick_profile"
                  return P_PickProfile
             ]

authUrlInverse :: Property
authUrlInverse =
    property (pathInfoInverse_prop :: ProfileURL -> Bool)

{-
instance EmbedAsAttr (RouteT ProfileURL (ServerPartT IO)) (Attr String ProfileURL) where
    asAttr (n := u) = 
        do url <- showURL u
           asAttr $ MkAttr (toName n, pAttrVal url)
-}