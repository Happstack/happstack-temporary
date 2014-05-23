{-# LANGUAGE DataKinds, DeriveDataTypeable, DeriveGeneric, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, StandaloneDeriving, TemplateHaskell, TypeFamilies, TypeSynonymInstances, UndecidableInstances, OverloadedStrings #-}
{-

A user is uniquely identified by their 'UserId'. A user can have one
or more authentication methods associated with their account. However,
each authentication method can only be associated with a single
'UserId'. This means, for example, that a user can not use the same
openid account to log in as multiple different users.

Additionally, it is assume that all authentication methods associated
with the 'UserId' are controlled by a single individual. They are not
intended to provide a way for several different users to share the
same account.

An email address is also collected to make account recovery easier.

-}

module Happstack.Authenticate.Core where

import Control.Applicative (Applicative(pure), Alternative, (<$>), optional)
import Control.Lens        (makeLenses, view)
import Control.Lens.At     (IxValue(..), Ixed(..), Index(..), At(at))
import Control.Monad.Trans (MonadIO(liftIO))
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put, modify)
import Crypto.PasswordStore          (genSaltIO, exportSalt, makePassword, verifyPassword)
import Data.Aeson                    (FromJSON, ToJSON)
import Data.Acid (AcidState, Update, Query, makeAcidic)
import Data.Acid.Advanced (update', query')
import Data.Data (Data, Typeable)
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Data.IxSet.Typed
import qualified Data.IxSet.Typed as IxSet
import           Data.Set (Set)
import qualified Data.Set as Set
import Data.Text     (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time     (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Happstack.Server (Cookie(secure), CookieLife(Session), Happstack, Request(rqSecure), addCookie, askRq, expireCookie, lookCookieValue, mkCookie)
import GHC.Generics  (Generic)
import Web.Routes    (PathInfo(..))
{-
-- | an 'AuthId' uniquely identifies an authentication.
newtype AuthId = AuthId { unAuthId :: Integer }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''AuthId)

instance PathInfo AuthId where
    toPathSegments (AuthId i) = toPathSegments i
    fromPathSegments = AuthId <$> fromPathSegments

succAuthId :: AuthId -> AuthId
succAuthId (AuthId i) = AuthId (succ i)

-}

------------------------------------------------------------------------------
-- UserId
------------------------------------------------------------------------------

-- | a 'UserId' uniquely identifies a user.
newtype UserId = UserId { _unUserId :: Integer }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

deriveSafeCopy 1 'base ''UserId
makeLenses ''UserId

instance FromJSON UserId
instance ToJSON UserId
instance PathInfo UserId where
    toPathSegments (UserId i) = toPathSegments i
    fromPathSegments = UserId <$> fromPathSegments

succUserId :: UserId -> UserId
succUserId (UserId i) = UserId (succ i)

------------------------------------------------------------------------------
-- Username
------------------------------------------------------------------------------

-- | an arbitrary, but unique string that the user uses to identify themselves
newtype Username = Username { _unUsername :: Text }
      deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)
deriveSafeCopy 1 'base ''Username
makeLenses ''Username

instance FromJSON Username
instance ToJSON Username
instance PathInfo Username where
    toPathSegments (Username t) = toPathSegments t
    fromPathSegments = Username <$> fromPathSegments

------------------------------------------------------------------------------
-- Email
------------------------------------------------------------------------------

newtype Email = Email { _unEmail :: Text }
      deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)
deriveSafeCopy 1 'base ''Email
makeLenses ''Email

instance FromJSON Email
instance ToJSON Email
instance PathInfo Email where
    toPathSegments (Email t) = toPathSegments t
    fromPathSegments = Email <$> fromPathSegments

------------------------------------------------------------------------------
-- User
------------------------------------------------------------------------------

-- | A unique 'User'
data User = User
    { _userId   :: UserId
    , _username :: Username
    , _email    :: Maybe Email
    }
      deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)
deriveSafeCopy 1 'base ''User
makeLenses ''User

instance FromJSON User
instance ToJSON User

type UserIxs = '[UserId, Username, Email]
type IxUser  = IxSet UserIxs User

instance Indexable UserIxs User where
    empty = mkEmpty
             (ixFun $ (:[]) . view userId)
             (ixFun $ (:[]) . view username)
             (ixFun $ maybeToList . view email)

{-
type instance IxValue (IxSet ixs a) = a
instance (Indexable ixs a, IsIndexOf (Index (IxSet ixs a)) ixs) => Ixed (IxSet ixs a) where
    ix k f s =
        case getOne $ s @= k of
          Nothing -> pure s
          (Just v) -> f v <&>  \v' -> IxSet.insert k v' s
-- instance At (IxSet ixs a)
-}

------------------------------------------------------------------------------
-- AuthToken
------------------------------------------------------------------------------


newtype AuthSecret = AuthSecret { _unAuthSecret :: Text }
      deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)
deriveSafeCopy 1 'base ''AuthSecret
makeLenses ''AuthSecret

instance FromJSON AuthSecret
instance ToJSON AuthSecret

data AuthToken = AuthToken
    { _tokenValue      :: AuthSecret         -- ^ secret text
    , _tokenExpires    :: UTCTime      -- ^ time this token will expire
    , _tokenLifetime   :: Int          -- ^ lifetime of token in seconds
    , _tokenUserId     :: UserId
--    , tokenAuthId     :: Maybe AuthId
--    , tokenAuthMethod :: AuthMethod
    }
      deriving (Eq, Ord, Show, Data, Typeable, Generic)
deriveSafeCopy 1 'base ''AuthToken
makeLenses ''AuthToken

instance FromJSON AuthToken
instance ToJSON AuthToken

type AuthTokenIxs = '[AuthSecret, UserId]
type IxAuthToken  = IxSet AuthTokenIxs AuthToken

instance Indexable AuthTokenIxs AuthToken where
    empty = mkEmpty (ixFun $ (:[]) . view tokenValue)
                    (ixFun $ (:[]) . view tokenUserId)

-- | generate an new authentication token
genAuthToken :: (MonadIO m) => UserId -> Int -> m AuthToken
genAuthToken userId lifetime = liftIO $
    do -- the docs promise that the salt will be base64, so 'Text.decodeUtf8' should be safe
       random <- Text.decodeUtf8 . exportSalt <$> genSaltIO
       now    <- getCurrentTime
       let expires = addUTCTime (fromIntegral lifetime) now
           prefix  = Text.pack (show (_unUserId userId))
       return $ AuthToken { _tokenValue    = AuthSecret $ prefix <> random
                          , _tokenExpires  = expires
                          , _tokenLifetime = lifetime
                          , _tokenUserId   = userId
                          }

------------------------------------------------------------------------------
-- AuthenticateState
------------------------------------------------------------------------------

data AuthenticateState = AuthenticateState
    { _authTokens            :: IxAuthToken
    , _users                 :: IxUser
    , _defaultSessionTimeout :: Int -- ^ default session time out in seconds
    }
    deriving (Eq, Show, Typeable, Generic)
deriveSafeCopy 1 'base ''AuthenticateState
makeLenses ''AuthenticateState

-- | a reasonable initial 'AuthenticateState'
initialAuthenticateState :: AuthenticateState
initialAuthenticateState = AuthenticateState
    { _authTokens             = IxSet.empty
    , _users                  = IxSet.empty
    , _defaultSessionTimeout  = 60*60
    }

------------------------------------------------------------------------------
-- AuthToken related AcidState functions
------------------------------------------------------------------------------

addAuthToken :: AuthToken -> Update AuthenticateState ()
addAuthToken authToken =
    do as@AuthenticateState{..} <- get
       put (as { _authTokens = IxSet.insert authToken _authTokens })

-- | look up the 'AuthToken' associated with the 'AuthSecret'
askAuthToken :: AuthSecret  -- ^ token (used in the cookie, etc)
             -> Query AuthenticateState (Maybe AuthToken)
askAuthToken secret =
    do as@AuthenticateState{..} <- ask
       return $ getOne $ _authTokens @= secret

-- | update the 'AuthToken'
updateAuthToken :: AuthToken -> Update AuthenticateState ()
updateAuthToken authToken =
    do as@AuthenticateState{..} <- get
       put (as { _authTokens = IxSet.updateIx (view tokenValue authToken) authToken _authTokens })

-- | delete the 'AuthToken' associated with the 'AuthSecret'
deleteAuthToken :: AuthSecret
                -> Update AuthenticateState ()
deleteAuthToken token =
    do as@AuthenticateState{..} <- get
       put (as { _authTokens = IxSet.deleteIx token _authTokens })

-- | purge any 'AuthToken' from the database that is past its expiration date
purgeExpiredTokens :: UTCTime
                   -> Update AuthenticateState ()
purgeExpiredTokens now =
    do as@AuthenticateState{..} <- get
       let authTokens' = IxSet.fromList $ filter (\at -> (view tokenExpires at) > now) (IxSet.toList _authTokens)
       put as { _authTokens = authTokens'}

------------------------------------------------------------------------------
-- SessionTimeout related AcidState functions
------------------------------------------------------------------------------

-- | set the default inactivity timeout for new sessions
setDefaultSessionTimeout :: Int -- ^ default timout in seconds (should be >= 180)
               -> Update AuthenticateState ()
setDefaultSessionTimeout newTimeout =
    modify $ \as@AuthenticateState{..} -> as { _defaultSessionTimeout = newTimeout }

-- | set the default inactivity timeout for new sessions
getDefaultSessionTimeout :: Query AuthenticateState Int
getDefaultSessionTimeout =
    view defaultSessionTimeout <$> ask

------------------------------------------------------------------------------
-- User related AcidState functions
------------------------------------------------------------------------------

-- | look up a 'User' by their 'Username'
getUserByUsername :: Username
                  -> Query AuthenticateState (Maybe User)
getUserByUsername username =
    do us <- view users
       return $ getOne $ us @= username

-- | look up a 'User' by their 'UserId'
getUserByUserId :: UserId
                  -> Query AuthenticateState (Maybe User)
getUserByUserId userId =
    do us <- view users
       return $ getOne $ us @= userId

-- | look up a 'User' by their 'Email'
getUserByEmail :: Email
               -> Query AuthenticateState (Maybe User)
getUserByEmail email =
    do us <- view users
       return $ getOne $ us @= email

makeAcidic ''AuthenticateState
    [ 'addAuthToken
    , 'askAuthToken
    , 'updateAuthToken
    , 'deleteAuthToken
    , 'purgeExpiredTokens
    , 'setDefaultSessionTimeout
    , 'getDefaultSessionTimeout
    , 'getUserByUsername
    , 'getUserByUserId
    , 'getUserByEmail
    ]

------------------------------------------------------------------------------
-- Functions
------------------------------------------------------------------------------

-- | add an authentication token for 'UserId'
addAuthCookie :: (Happstack m) =>
                 AcidState AuthenticateState
              -> UserId
              -> m ()
addAuthCookie acidH userId =
    do lifetime  <- query' acidH GetDefaultSessionTimeout
       authToken <- genAuthToken userId lifetime
       now <- liftIO $ getCurrentTime
       update' acidH (PurgeExpiredTokens now)
       update' acidH (AddAuthToken authToken)
       s <- rqSecure <$> askRq
       addCookie Session ((mkCookie "authToken" (Text.unpack $ view unAuthSecret $ (view tokenValue authToken))) { secure = s })
       return ()

-- | delete the authentication token for a 'UserId'
deleteAuthCookie :: (Happstack m, Alternative m) =>
                    AcidState AuthenticateState
                 -> m ()
deleteAuthCookie acidH =
    do mToken <- optional $ (AuthSecret . Text.pack) <$> lookCookieValue "authToken"
       case mToken of
         Nothing         -> return ()
         (Just token) ->
             do expireCookie "authToken"
                update' acidH (DeleteAuthToken token)

getAuthToken :: (Alternative m, Happstack m) =>
                AcidState AuthenticateState
             -> m (Maybe AuthToken)
getAuthToken acidH =
    do mToken <- optional $ (AuthSecret . Text.pack) <$> lookCookieValue "authToken"
       case mToken of
         Nothing         -> return Nothing
         (Just token) ->
           do mAuthToken <- query' acidH (AskAuthToken token)
              case mAuthToken of
                Nothing -> return Nothing
                (Just authToken) ->
                    do now <- liftIO $ getCurrentTime
                       if now > (view tokenExpires authToken)
                         then do expireCookie "authToken"
                                 update' acidH (DeleteAuthToken token)
                                 return Nothing
                         else if (diffUTCTime (addUTCTime (fromIntegral $ view tokenLifetime authToken) now) (view tokenExpires authToken)) > 60
                                then do let newAuthToken = authToken { _tokenExpires = addUTCTime (fromIntegral $ view tokenLifetime authToken) now }
                                        update' acidH (UpdateAuthToken newAuthToken)
                                        return (Just newAuthToken)
                                else return (Just authToken)

-- | get the 'UserId', assuming the user has a value authentication token
getUserId :: (Alternative m, Happstack m) =>
             AcidState AuthenticateState
          -> m (Maybe UserId)
getUserId acidH =
    do mAuthToken <- getAuthToken acidH
       case mAuthToken of
         Nothing -> return Nothing
         (Just authToken) ->
             return $ Just (view tokenUserId authToken)
