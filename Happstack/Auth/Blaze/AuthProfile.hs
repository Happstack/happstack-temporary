{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
-- |This module provides a higher level wrapper around handleAuth + handleProfile. 
module Happstack.Auth.Blaze.AuthProfile where

import Control.Applicative ((<$>))
import Control.Monad
import Data.Acid
import Data.Data
import Happstack.Auth.Core.Auth
import Happstack.Auth.Core.AuthParts
import Happstack.Auth.Core.AuthURL
import Happstack.Auth.Core.Profile
import Happstack.Auth.Core.ProfileParts
import Happstack.Auth.Core.ProfileURL
import Happstack.Auth.Blaze.Login
import Happstack.Server
import Happstack.Server.HSP.HTML
import HSP
import qualified HSX.XMLGenerator as HSX
import Web.Authenticate.Facebook (Facebook)
import Web.Routes
import Web.Routes.XMLGenT
import Web.Routes.Happstack
import Test.QuickCheck

data AuthProfileURL
    = AP_Auth AuthURL
    | AP_Profile ProfileURL
      deriving (Eq, Ord, Read, Show, Data, Typeable)

instance PathInfo AuthProfileURL where
    toPathSegments (AP_Auth authURL)       = "auth"    : toPathSegments authURL
    toPathSegments (AP_Profile profileURL) = "profile" : toPathSegments profileURL

    fromPathSegments =
        msum [ do segment "auth"
                  AP_Auth <$> fromPathSegments
             , do segment "profile"
                  AP_Profile <$> fromPathSegments
             ]

instance Arbitrary AuthProfileURL where
    arbitrary = oneof [ AP_Auth <$> arbitrary
                      , AP_Profile <$> arbitrary
                      ]

authProfileUrlInverse :: Property
authProfileUrlInverse =
    property (pathInfoInverse_prop :: AuthProfileURL -> Bool)


handleAuthProfile :: Happstack m =>
                     AcidState AuthState
                  -> AcidState ProfileState
                  -> (String -> [XML] -> [XML] -> RouteT AuthProfileURL m Response)
                  -> Maybe Facebook
                  -> Maybe String
                  -> String
                  -> AuthProfileURL
                  -> RouteT AuthProfileURL m Response
handleAuthProfile acidAuth acidProfile appTemplate mFacebook realm postPickedURL url =
    case url of
      (AP_Auth authURL) -> 
          do onAuthURL <- showURL (AP_Profile P_PickProfile)
             showFn <- askRouteT
             let template = urlTemplate appTemplate showFn
             nestURL AP_Auth $ handleAuth acidAuth template mFacebook realm onAuthURL authURL

      (AP_Profile profileURL) ->
          do showFn <- askRouteT
             let template = urlTemplate appTemplate showFn
             nestURL AP_Profile $ handleProfile acidAuth acidProfile template postPickedURL profileURL

authProfileSite :: (Happstack m) =>
                   AcidState AuthState
                -> AcidState ProfileState
                -> (String  -> [XML] -> [XML] -> RouteT AuthProfileURL m Response)
                -> Maybe Facebook
                -> Maybe String
                -> String
                -> Site AuthProfileURL (m Response)
authProfileSite acidAuth acidProfile appTemplate mFacebook realm postPickedURL
    = Site { handleSite = \f u -> unRouteT (handleAuthProfile acidAuth acidProfile appTemplate mFacebook realm postPickedURL u) f
           , formatPathSegments = \u -> (toPathSegments u, [])
           , parsePathSegments  = parseSegments fromPathSegments
           }

authProfileHandler :: (Happstack m) =>
                      String
                   -> String
                   -> AcidState AuthState
                   -> AcidState ProfileState
                   -> (String  -> [XML] -> [XML] -> RouteT AuthProfileURL m Response)
                   -> Maybe Facebook
                   -> Maybe String
                   -> String
                   -> m Response
authProfileHandler baseURI pathPrefix acidAuth acidProfile appTemplate mFacebook realm postPickedURL =
    do r <- implSite_ baseURI pathPrefix (authProfileSite acidAuth acidProfile appTemplate mFacebook realm postPickedURL)
       case r of
         (Left e) -> mzero
         (Right r) -> return r

urlTemplate :: ( EmbedAsChild (RouteT url m) headers
               , EmbedAsChild (RouteT url m) body
               ) =>
               (String -> [XML] -> [XML] -> RouteT AuthProfileURL m Response)
            -> (AuthProfileURL -> [(String, String)] -> String)
            -> String
            -> headers
            -> body
            -> RouteT url m Response
urlTemplate appTemplate showFn title headers body =
    do headersXML <- unXMLGenT $ asChild headers
       bodyXML    <- unXMLGenT $ asChild body
       unnest showFn $ appTemplate title (map unUChild headersXML) (map unUChild bodyXML)


unnest :: (AuthProfileURL -> [(String, String)] -> String)
       -> RouteT AuthProfileURL m Response 
       -> RouteT url m Response
unnest showFn routeSP =
    liftRouteT $ unRouteT routeSP showFn

