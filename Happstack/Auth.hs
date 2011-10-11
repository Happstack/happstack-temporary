module Happstack.Auth 
    ( UserId(..)
    , getUserId
    , handleAuth
    , handleProfile
    , handleAuthProfile
    ) where

import Happstack.Auth.Core.Profile    (UserId(..), getUserId)
import Happstack.Auth.Blaze.Templates (handleAuth, handleProfile)