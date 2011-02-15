{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances #-}
module ProfileURL where

import Control.Applicative ((<$>))
import Control.Monad
import Data.Data
import Data.Typeable
import Test.QuickCheck
import Web.Routes
import Happstack.Server
import Web.Routes.Happstack
import Web.Routes.MTL
import Web.Routes.XMLGenT
import Types

instance Happstack (RouteT ProfileURL (ServerPartT IO))

data ProfileURL
    = P_SetPersonality UserId
    | P_PickProfile
      deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Arbitrary ProfileURL where
    arbitrary = oneof $ [ P_SetPersonality . UserId <$> arbitrary
                        , return P_PickProfile
                        ]


instance PathInfo ProfileURL where
    toPathSegments (P_SetPersonality userId) = "set_personality" : toPathSegments userId
    toPathSegments P_PickProfile             = ["pick_profile"]

    fromPathSegments =
        msum [ do segment "set_personality"
                  userId <- fromPathSegments
                  return (P_SetPersonality userId)
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