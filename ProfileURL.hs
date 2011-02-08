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
import Types

instance Happstack (RouteT ProfileURL (ServerPartT IO))

data ProfileURL
    = P_SetPersonality UserId
      deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Arbitrary ProfileURL where
    arbitrary = oneof $ [ P_SetPersonality . UserId <$> arbitrary
                        ]


instance PathInfo ProfileURL where
    toPathSegments (P_SetPersonality userId) = "set_personality" : toPathSegments userId

    fromPathSegments =
        msum [ do segment "set_personality"
                  userId <- fromPathSegments
                  return (P_SetPersonality userId)
             ]

authUrlInverse :: Property
authUrlInverse =
    property (pathInfoInverse_prop :: ProfileURL -> Bool)
