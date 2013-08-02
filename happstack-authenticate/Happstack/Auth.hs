module Happstack.Auth
    ( UserId(..)
    , AuthState(..)
    , ProfileState(..)
    , AuthProfileURL(..)
    , AuthURL(..)
    , ProfileURL(..)
    , getUserId
    , authProfileHandler
    , handleAuth
    , handleProfile
    , handleAuthProfile
    , handleAuthProfileRouteT
    ) where

import Happstack.Auth.Core.Profile    (UserId(..), getUserId)
import Happstack.Auth.Core.Auth       (AuthState(..))
import Happstack.Auth.Core.AuthURL    (AuthURL(..))
import Happstack.Auth.Core.Profile    (ProfileState(..))
import Happstack.Auth.Core.ProfileURL (ProfileURL(..))
import Happstack.Auth.Core.AuthProfileURL (AuthProfileURL(..))
import Happstack.Auth.Blaze.Templates (authProfileHandler, handleAuth, handleProfile, handleAuthProfile, handleAuthProfileRouteT)