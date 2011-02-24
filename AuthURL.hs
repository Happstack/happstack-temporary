{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances #-}
module AuthURL where

import Control.Applicative ((<$>), (<*>))
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

instance PathInfo OpenIdProvider where
    toPathSegments Google      = ["google"]
    toPathSegments Yahoo       = ["yahoo"]
    toPathSegments Myspace     = ["myspace"]
    toPathSegments LiveJournal = ["livejournal"]
    toPathSegments Generic     = ["generic"]
    fromPathSegments =
        msum [ do segment "google"
                  return Google
             , do segment "yahoo"
                  return Yahoo
             , do segment "myspace"
                  return Myspace
             , do segment "livejournal"
                  return LiveJournal
             , do segment "generic"
                  return Generic
             ]

instance Arbitrary OpenIdProvider where
    arbitrary = oneof $ map return [ minBound .. maxBound ]

data AuthMode 
    = LoginMode
    | AddIdentifierMode
      deriving (Eq, Ord, Read, Show, Data, Typeable)

instance PathInfo AuthMode where
    toPathSegments LoginMode         = ["login"]
    toPathSegments AddIdentifierMode = ["add_identifier"]
    fromPathSegments =
        msum [ do segment "login"
                  return LoginMode
             , do segment "add_identifier"
                  return AddIdentifierMode
             ]
    
      
instance Arbitrary AuthMode where
    arbitrary = oneof [ return LoginMode
                      , return AddIdentifierMode
                      ]

data AuthURL 
    = A_Login
    | A_AddAuth
    | A_Logout
    | A_Local
    | A_OpenId AuthMode
    | A_OpenIdProvider AuthMode OpenIdProvider
    | A_CreateAccount
    | A_ChangePassword
      deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Arbitrary AuthURL where
    arbitrary = oneof [ return A_Login
                      , return A_AddAuth
                      , return A_Logout
                      , return A_Local
                      , A_OpenId <$> arbitrary
                      , A_OpenIdProvider <$> arbitrary <*> arbitrary
                      , return A_CreateAccount
                      , return A_ChangePassword
                      ]


-- $(derivePathInfo ''AuthURL)

instance PathInfo AuthURL where
    toPathSegments A_Login                        = ["login"]
    toPathSegments A_AddAuth                      = ["add_auth"]
    toPathSegments A_Logout                       = ["logout"]
    toPathSegments A_Local                        = ["local"]
    toPathSegments (A_OpenId authMode)            = "openid_return" : toPathSegments authMode
    toPathSegments (A_OpenIdProvider authMode provider) 
                                                  = "openid" : toPathSegments authMode ++ toPathSegments provider
    toPathSegments A_CreateAccount                = ["create"]
    toPathSegments A_ChangePassword               = ["change_password"]

    fromPathSegments =
        msum [ do segment "login"
                  return A_Login
             , do segment "add_auth"
                  return A_AddAuth
             , do segment "logout"
                  return A_Logout
             , do segment "local"
                  return A_Local
             , do segment "openid_return"
                  mode <- fromPathSegments
                  return (A_OpenId mode)
             , do segment "openid"
                  authMode <- fromPathSegments
                  provider <- fromPathSegments
                  return (A_OpenIdProvider authMode provider)
             , do segment "create"
                  return A_CreateAccount
             , do segment "change_password"
                  return A_ChangePassword
             ]

authUrlInverse :: Property
authUrlInverse =
    property (pathInfoInverse_prop :: AuthURL -> Bool)
