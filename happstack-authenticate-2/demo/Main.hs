{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, FlexibleInstances, OverlappingInstances, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Exception (bracket, finally)
import Control.Concurrent.Thread.Group (ThreadGroup, new)
import Control.Lens (makeLenses)
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Data.Acid (AcidState)
import Data.Acid.Local (openLocalStateFrom, createCheckpointAndClose)
import Data.Aeson
import Data.Aeson.Types (ToJSON(..), FromJSON(..), Options(fieldLabelModifier), defaultOptions, genericToJSON, genericParseJSON)
import Data.Data
import Data.Default (def)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Unique
import Data.Monoid ((<>))
import GHC.Generics
import Happstack.Authenticate.Core (AuthenticateURL(..), AuthenticateState, Email(..), User(..), Username(..), UserId(..), initAuthentication, decodeAndVerifyToken)
import Happstack.Authenticate.Password.Controllers(usernamePasswordCtrl)
import Happstack.Authenticate.Password.Core(PasswordState)
import Happstack.Authenticate.Password.Route (initPassword)
import Happstack.Authenticate.Password.URL(PasswordURL(..))
import Happstack.Server
import Happstack.Server.HSP.HTML
import Happstack.Server.XMLGenT
import Happstack.Server.JMacro ()
import HSP
import HSP.Monad
import HSP.JMacro
import Language.Haskell.HSX.QQ (hsx)
import Language.Javascript.JMacro
import Text.PrettyPrint.Leijen.Text        (Doc, displayT, renderOneLine)
import Web.JWT (Algorithm(HS256), JWTClaimsSet(..), encodeSigned, secret, decodeAndVerifySignature)
import Web.Routes
import Web.Routes.Happstack
import Web.Routes.TH

------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- Type-Safe URLs
------------------------------------------------------------------------------
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- route types
------------------------------------------------------------------------------


data API
  = Restricted
    deriving (Eq, Ord, Data, Typeable, Generic)

derivePathInfo ''API

data SiteURL
  = Index
  | Authenticate AuthenticateURL
  | Api API
  | DemoAppJs
  | UsernamePasswordJs
    deriving (Eq, Ord, Data, Typeable, Generic)

derivePathInfo ''SiteURL

------------------------------------------------------------------------------
-- route functions
------------------------------------------------------------------------------

route :: AcidState AuthenticateState
      -> (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response)
      -> SiteURL
      -> RouteT SiteURL (ServerPartT IO) Response
route authenticateState routeAuthenticate url =
  case url of
    Index        -> index
    Authenticate authenticateURL -> nestURL Authenticate $ routeAuthenticate authenticateURL
    DemoAppJs   ->
      do ok $ toResponse $ demoAppJs
    UsernamePasswordJs ->
         do js <- nestURL Authenticate $ usernamePasswordCtrl
            ok $ toResponse js
--         ok $ toResponse $ userCtrl (u -> routeFn (Authenticate (

    Api Restricted -> lift (api authenticateState)

api :: AcidState AuthenticateState
    -> ServerPartT IO Response
api authenticateState =
  do mAuth <- getHeaderM "Authorization"
     case mAuth of
       Nothing -> unauthorized $ toResponse ("You are not authorized." :: Text)
       (Just auth') ->
         do let auth = B.drop 7 auth'
            mToken <- decodeAndVerifyToken authenticateState (T.decodeUtf8 auth)
            case mToken of
              Nothing -> unauthorized $ toResponse ("You are not authorized." :: Text)
              (Just (_user, jwt)) ->
                  ok $ toJSONResponse $ Object $ HashMap.fromList [("name", toJSON (show jwt))]

------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- JSON/Javascript helpers
------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | when creating JSON field names, drop the first character. Since
-- we are using lens, the leading character should always be _.
jsonOptions :: Options
jsonOptions = defaultOptions { fieldLabelModifier = drop 1 }

toJSONResponse :: Value -> Response
toJSONResponse v = toResponseBS "application/json" (encode v)

------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- Client-side Controllers
------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | app module for angulasjs
--
-- We just depend on the usernamePassword module
demoAppJs :: JStat
demoAppJs = [jmacro|
  {
    var demoApp = angular.module('demoApp', [
      'happstackAuthentication',
      'usernamePassword'
    ]);

    demoApp.controller('DemoAppCtrl', ['$scope', '$http',function($scope, $http) {
      $scope.message = '';

      $scope.callRestricted = function (url) {
        $http({url: url, method: 'GET'}).
        success(function (datum, status, headers, config) {
          $scope.message = datum.name;
        }).
        error(function (datum, status, headers, config) {
          alert(datum);
        });
      };
    }]);
  }
 |]

------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- Views
------------------------------------------------------------------------------
------------------------------------------------------------------------------

simpleView :: (Happstack m) =>
              XMLGenT (RouteT SiteURL m) XML
           -> RouteT SiteURL m Response
simpleView hsx =
  do xml <- unXMLGenT hsx
     ok $ toResponse xml

index :: RouteT SiteURL (ServerPartT IO) Response
index = do
  routeFn <- askRouteFn
  simpleView [hsx|
    <html>
      <head>
        <meta http-equiv="content-type" content="text/html; charset=utf-8" />
        <meta http-equiv="X-UA-Compatible" content="IE=edge" />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <title>Happstack Authenticate Demo w/Angular + Bootstrap</title>
        <link rel="stylesheet" href="/bootstrap/css/bootstrap.min.css" />
--        <script src="//ajax.googleapis.com/ajax/libs/angularjs/1.2.7/angular.min.js"></script>
        <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"></script>
        <script src="/bootstrap/js/bootstrap.min.js"></script>
        <script src="/js/angular.min.js"></script>
        <script src=(routeFn DemoAppJs [])></script>
        <script src=(routeFn UsernamePasswordJs [])></script>
      </head>
      <body ng-app="demoApp">
       <div class="container-fluid">
       <div class="row">
       <div class="col-md-12">
        <div ng-controller="UsernamePasswordCtrl">
--         <% mapXMLGenT (nestURL Authenticate) usernamePasswordForm %>
         <up-login-inline />
         <div up-authenticated=False>
          <p>Not Authenticated</p>
         </div>
         <div up-authenticated=True>
          <p>Authenticated</p>
         </div>
         <div ng-controller="DemoAppCtrl">
          <a ng-click=("callRestricted('" <> (routeFn (Api Restricted) []) <> "')") href="">Shh, this is private!</a>
          <br />
          <div>{{message}}</div>
         </div>
        </div>
       </div>
       </div>
       </div>
      </body>
    </html>
  |]

main :: IO ()
main =
  do (cleanup, routeAuthenticate, authenticateState) <- initAuthentication Nothing [initPassword]
     (simpleHTTP nullConf $
      msum [ dir "js"        $ serveDirectory EnableBrowsing [] "js"
           , dir "bootstrap" $ serveDirectory EnableBrowsing [] "bootstrap"
           , implSite "//localhost:8000" "" $
              setDefault Index $ mkSitePI (runRouteT $ route authenticateState routeAuthenticate)
           ]) `finally` cleanup
