{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances #-}
module AuthURL where

import Control.Applicative ((<$>))
import Control.Monad
import Data.Data
import Data.Typeable
import Test.QuickCheck
import Web.Routes
import Happstack.Server
import Web.Routes.Happstack
import Web.Routes.MTL

-- | move to Web.Routes.Happstack
seeOtherURL :: (ShowURL m, FilterMonad Response m) => URL m -> m Response
seeOtherURL url = 
    do otherURL <- showURL url
       seeOther otherURL (toResponse "")

data OpenIdProvider
    = Google
    | Yahoo
    | Myspace
    | LiveJournal
    | Generic
      deriving (Eq, Ord, Read, Show, Data, Typeable, Enum, Bounded)

instance Arbitrary OpenIdProvider where
    arbitrary = oneof $ map return [ minBound .. maxBound ]


data AuthURL 
    = A_Login
    | A_Logout
    | A_Local
    | A_OpenId
    | A_OpenIdProvider OpenIdProvider
    | A_CreateAccount
    | A_ChangePassword
      deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Arbitrary AuthURL where
    arbitrary = oneof $ [ return A_Login
                        , return A_Logout
                        , return A_Local
                        , return A_OpenId
                        , A_OpenIdProvider <$> arbitrary
                        , return A_CreateAccount
                        , return A_ChangePassword
                        ]


-- $(derivePathInfo ''AuthURL)

instance PathInfo AuthURL where
    toPathSegments A_Login                        = ["login"]
    toPathSegments A_Logout                       = ["logout"]
    toPathSegments A_Local                        = ["local"]
    toPathSegments A_OpenId                       = ["openid_return"]
    toPathSegments (A_OpenIdProvider Google)      = ["openid", "google"]
    toPathSegments (A_OpenIdProvider Yahoo)       = ["openid", "yahoo"]
    toPathSegments (A_OpenIdProvider Myspace)     = ["openid", "myspace"]
    toPathSegments (A_OpenIdProvider LiveJournal) = ["openid", "livejournal"]
    toPathSegments (A_OpenIdProvider Generic)     = ["openid", "generic"]
    toPathSegments A_CreateAccount                = ["create"]
    toPathSegments A_ChangePassword               = ["change_password"]

    fromPathSegments =
        msum [ do segment "login"
                  return A_Login
             , do segment "logout"
                  return A_Logout
             , do segment "local"
                  return A_Local
             , do segment "openid_return"
                  return A_OpenId
             , do segment "openid"
                  msum [ do segment "google"
                            return (A_OpenIdProvider Google)
                       , do segment "yahoo"
                            return (A_OpenIdProvider Yahoo)
                       , do segment "myspace"
                            return (A_OpenIdProvider Myspace)
                       , do segment "livejournal"
                            return (A_OpenIdProvider LiveJournal)
                       , do segment "generic"
                            return (A_OpenIdProvider Generic)
                       ]
             , do segment "create"
                  return A_CreateAccount
             , do segment "change_password"
                  return A_ChangePassword
             ]

authUrlInverse :: Property
authUrlInverse =
    property (pathInfoInverse_prop :: AuthURL -> Bool)
