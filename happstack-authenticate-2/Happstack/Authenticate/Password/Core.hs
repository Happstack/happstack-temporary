{-# LANGUAGE DataKinds, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, TemplateHaskell, TypeFamilies, TypeSynonymInstances, OverloadedStrings #-}
module Happstack.Authenticate.Password.Core where

import Control.Applicative ((<$>))
import Control.Monad.Trans (MonadIO(..))
import Control.Lens  ((?~), (^.), (.=), (?=), assign, makeLenses, set, use, view, over)
import Control.Lens.At (at)
import Crypto.PasswordStore          (genSaltIO, exportSalt, makePassword, verifyPassword)
import Data.Acid          (AcidState, Query, Update, closeAcidState, makeAcidic)
import Data.Acid.Advanced (query', update')
import Data.Acid.Local    (createCheckpointAndClose, openLocalStateFrom)
import Data.Aeson         (Value(..), Object(..), decode, encode)
import Data.Aeson.Types   (ToJSON(..), FromJSON(..), Options(fieldLabelModifier), defaultOptions, genericToJSON, genericParseJSON)
import Data.ByteString (ByteString)
import Data.Data (Data, Typeable)
import qualified Data.HashMap.Strict as HashMap
import           Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe         (fromMaybe)
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)
import Happstack.Authenticate.Core (AuthenticationHandler, AuthenticationMethod(..), AuthenticateState(..), AuthenticateURL, CreateUser(..), Email(..), GetUserByUsername(..), UserId(..), User(..), Username(..), issueToken, jsonOptions, nestAuthenticationMethod, userId, toJSONResponse)
import Happstack.Server
import HSP.JMacro
import Language.Javascript.JMacro
import System.FilePath                 (combine)
import Web.Routes
import Web.Routes.TH

------------------------------------------------------------------------------
-- passwordAuthenticationMethod
------------------------------------------------------------------------------

passwordAuthenticationMethod :: AuthenticationMethod
passwordAuthenticationMethod = AuthenticationMethod "password"

------------------------------------------------------------------------------
-- PasswordError
------------------------------------------------------------------------------

data PasswordError
  = JSONDecodeFailed
  | URLDecodeFailed
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)
instance ToJSON   PasswordError where toJSON    = genericToJSON    jsonOptions
instance FromJSON PasswordError where parseJSON = genericParseJSON jsonOptions

instance ToJExpr PasswordError where
    toJExpr = toJExpr . toJSON

------------------------------------------------------------------------------
-- PasswordURL
------------------------------------------------------------------------------

data PasswordURL
  = Token
  | Account
  deriving (Eq, Ord, Data, Typeable, Generic)

derivePathInfo ''PasswordURL

-- showPasswordURL :: (MonadRoute m) => PasswordURL -> m Text
nestPasswordURL :: RouteT PasswordURL m a -> RouteT AuthenticateURL m a
nestPasswordURL =
  nestAuthenticationMethod passwordAuthenticationMethod

------------------------------------------------------------------------------
-- HashedPass
------------------------------------------------------------------------------

newtype HashedPass = HashedPass { _unHashedPass :: ByteString }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)
deriveSafeCopy 1 'base ''HashedPass
makeLenses ''HashedPass

-- | hash a password string
mkHashedPass :: (Functor m, MonadIO m) =>
                Text         -- ^ password in plain text
             -> m HashedPass -- ^ salted and hashed
mkHashedPass pass = HashedPass <$> (liftIO $ makePassword (Text.encodeUtf8 pass) 12)

-- | verify a password
verifyHashedPass :: Text       -- ^ password in plain text
                 -> HashedPass -- ^ hashed version of password
                 -> Bool
verifyHashedPass passwd (HashedPass hashedPass) =
    verifyPassword (Text.encodeUtf8 passwd) hashedPass

------------------------------------------------------------------------------
-- PasswordState
------------------------------------------------------------------------------

data PasswordState = PasswordState
    { _passwords :: Map UserId HashedPass
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)
deriveSafeCopy 1 'base ''PasswordState
makeLenses ''PasswordState

initialPasswordState :: PasswordState
initialPasswordState = PasswordState
    { _passwords = Map.fromList [(UserId 0, HashedPass {_unHashedPass = "sha256|12|yR1jCpIvCOrhH285/QMwVw==|oSJVi3fypHEqtjXrtDU/iNApqcbwhxngW0bdhXmMS2A="})]
    }

------------------------------------------------------------------------------
-- AcidState PasswordState queries/updates
------------------------------------------------------------------------------

-- | set the password for 'UserId'
setPassword :: UserId     -- ^ UserId
            -> HashedPass -- ^ the hashed password
            -> Update PasswordState ()
setPassword userId hashedPass =
    passwords . at userId ?= hashedPass

-- | delete the password for 'UserId'
deletePassword :: UserId     -- ^ UserId
            -> Update PasswordState ()
deletePassword userId =
    passwords . at userId .= Nothing

-- | verify that the supplied password matches the stored hashed password for 'UserId'
verifyPasswordForUserId :: UserId -- ^ UserId
                        -> Text   -- ^ plain-text password
                        -> Query PasswordState Bool
verifyPasswordForUserId userId plainPassword =
    do mHashed <- view (passwords . at userId)
       case mHashed of
         Nothing       -> return False
         (Just hashed) -> return (verifyHashedPass plainPassword hashed)

makeAcidic ''PasswordState
    [ 'setPassword
    , 'deletePassword
    , 'verifyPasswordForUserId
    ]

------------------------------------------------------------------------------
-- Functions
------------------------------------------------------------------------------

-- | verify that the supplied username/password is valid
verifyPassword :: (MonadIO m) =>
                  AcidState AuthenticateState
               -> AcidState PasswordState
               -> Username
               -> Text
               -> m Bool
verifyPassword authenticateState passwordState username password =
    do mUser <- query' authenticateState (GetUserByUsername username)
       case mUser of
         Nothing -> return False
         (Just user) ->
             query' passwordState (VerifyPasswordForUserId (view userId user) password)


------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------

data UserPass = UserPass
    { _user     :: Username
    , _password :: Text
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)
makeLenses ''UserPass
instance ToJSON   UserPass where toJSON    = genericToJSON    jsonOptions
instance FromJSON UserPass where parseJSON = genericParseJSON jsonOptions

instance ToJExpr UserPass where
    toJExpr = toJExpr . toJSON


token :: (Happstack m) =>
         AcidState AuthenticateState
      -> AcidState PasswordState
      -> m Response
token authenticateState passwordState =
  do method POST
     (Just (Body body)) <- takeRequestBody =<< askRq
     case decode body of
       Nothing   -> unauthorized (toResponse ("Invalid data." :: Text))
       (Just (UserPass username password)) ->
         do mUser <- query' authenticateState (GetUserByUsername username)
            case mUser of
              Nothing -> unauthorized (toResponse ("Invalid Username or Password" :: Text))
              (Just u) ->
                do valid <- query' passwordState (VerifyPasswordForUserId (u ^. userId) password)
                   if not valid
                     then unauthorized (toResponse ("Invalid Username or Password" :: Text))
                     else do token <- issueToken authenticateState u
                             resp 201 $ toResponseBS "application/json" $ encode $ Object $ HashMap.fromList [("token", toJSON token)]

data NewAccount = NewAccount
    { _naUser     :: User
    , _naPassword :: Text
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)
makeLenses ''NewAccount
instance ToJSON   NewAccount where toJSON    = genericToJSON    jsonOptions
instance FromJSON NewAccount where parseJSON = genericParseJSON jsonOptions

account :: (Happstack m) =>
           AcidState AuthenticateState
        -> AcidState PasswordState
        -> m (Either PasswordError UserId)
account authenticateState passwordState =
  do method POST
     (Just (Body body)) <- takeRequestBody =<< askRq
     case decode body of
       Nothing               -> badRequest (Left JSONDecodeFailed)
       (Just newAccount) ->
         do user <- update' authenticateState (CreateUser $ _naUser newAccount)
            hashed <- mkHashedPass (_naPassword newAccount)
            update' passwordState (SetPassword (user ^. userId) hashed)
            ok (Right (user ^. userId))

------------------------------------------------------------------------------
-- routePassword
------------------------------------------------------------------------------

routePassword :: (Happstack m) =>
                 AcidState AuthenticateState
              -> AcidState PasswordState
              -> [Text]
              -> m Response
routePassword authenticateState passwordState pathSegments =
  case parseSegments fromPathSegments pathSegments of
    (Left _) -> notFound $ toJSONResponse URLDecodeFailed
    (Right url) ->
      case url of
        Token   -> token authenticateState passwordState
        Account -> toJSONResponse <$> account authenticateState passwordState


------------------------------------------------------------------------------
-- initPassword
------------------------------------------------------------------------------

initPassword :: FilePath
             -> AcidState AuthenticateState
             -> IO (Bool -> IO (), (AuthenticationMethod, AuthenticationHandler))
initPassword basePath authenticateState =
  do passwordState <- openLocalStateFrom (combine basePath "password") initialPasswordState
     let shutdown = \normal ->
           if normal
           then createCheckpointAndClose passwordState
           else closeAcidState passwordState
         authenticationHandler =
           routePassword authenticateState passwordState
     return (shutdown, (passwordAuthenticationMethod, authenticationHandler))
