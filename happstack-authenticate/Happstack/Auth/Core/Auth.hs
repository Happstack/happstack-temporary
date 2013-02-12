{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeSynonymInstances, DeriveDataTypeable,
    FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators, RecordWildCards, StandaloneDeriving
    #-}
module Happstack.Auth.Core.Auth
    ( UserPass(..)
    , UserPassId(..)
    , UserName(..)
    , UserPassError(..)
    , userPassErrorString
    , SetUserName(..)
    , AuthState(..)
    , initialAuthState
    , AuthToken(..)
    , AuthId(..)
    , FacebookId(..)
    , AuthMethod(..)
    , AuthMethod_v1(..)
    , AuthMap(..)
    , HashedPass(..)
    , mkHashedPass
    , genAuthToken
    , AskUserPass(..)
    , CheckUserPass(..)
    , CreateUserPass(..)
    , SetPassword(..)
    , AddAuthToken(..)
    , AskAuthToken(..)
    , UpdateAuthToken(..)
    , DeleteAuthToken(..)
    , GenAuthId(..)
    , AddAuthMethod(..)
    , NewAuthMethod(..)
    , RemoveAuthIdentifier(..)
    , IdentifierAuthIds(..)
    , FacebookAuthIds(..)
    , AddAuthUserPassId(..)
    , RemoveAuthUserPassId(..)
    , UserPassIdAuthIds(..)
    , AskAuthState(..)
    , addAuthCookie
    , deleteAuthCookie
    , getAuthId
    , getAuthToken
    ) where

import Control.Applicative           (Alternative, (<$>), optional)
import Control.Monad                 (replicateM)
import Control.Monad.Reader          (ask)
import Control.Monad.State           (get, put, modify)
import Control.Monad.Trans           (MonadIO(..))
import Crypto.PasswordStore
import Data.Acid
import Data.Acid.Advanced            (query', update')
import Data.ByteString               (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Data                     (Data, Typeable)
import qualified Data.IxSet          as IxSet
import           Data.IxSet          (Indexable(..), IxSet, (@=), inferIxSet, noCalcs, inferIxSet, ixFun, ixSet, noCalcs, getOne, updateIx)
import Data.Map                      (Map)
import qualified Data.Map            as Map
import Data.SafeCopy -- (base, deriveSafeCopy)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import Data.Time.Clock               (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import qualified Data.Text           as Text
import qualified Data.Text.Encoding  as Text
import           Data.Text           (Text)
import Facebook                      (UserId, Id(..))
import Web.Authenticate.OpenId       (Identifier)
import Web.Routes                    (PathInfo(..))
import Happstack.Server              (Cookie(..), CookieLife(..), Happstack, Request(rqSecure), addCookie, askRq, expireCookie, lookCookieValue, mkCookie)

newtype AuthId = AuthId { unAuthId :: Integer }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''AuthId)

instance PathInfo AuthId where
    toPathSegments (AuthId i) = toPathSegments i
    fromPathSegments = AuthId <$> fromPathSegments

succAuthId :: AuthId -> AuthId
succAuthId (AuthId i) = AuthId (succ i)

-- * UserPass

newtype HashedPass = HashedPass ByteString
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''HashedPass)

-- | NOTE: The Eq and Ord instances are 'case-insensitive'. They apply 'toCaseFold' before comparing.
newtype UserName = UserName { unUserName :: Text }
    deriving (Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''UserName)

instance Eq UserName where
    (UserName x) == (UserName y) = (Text.toCaseFold x) == (Text.toCaseFold y)
    (UserName x) /= (UserName y) = (Text.toCaseFold x) /= (Text.toCaseFold y)

instance Ord UserName where
    compare (UserName x) (UserName y) = compare (Text.toCaseFold x) (Text.toCaseFold y)
    (UserName x) <  (UserName y)      = (Text.toCaseFold x) <  (Text.toCaseFold y)
    (UserName x) >= (UserName y)      = (Text.toCaseFold x) >= (Text.toCaseFold y)
    (UserName x) >  (UserName y)      = (Text.toCaseFold x) >  (Text.toCaseFold y)
    (UserName x) <= (UserName y)      = (Text.toCaseFold x) <= (Text.toCaseFold y)
    max (UserName x) (UserName y)     = UserName $ max (Text.toCaseFold x) (Text.toCaseFold y)
    min (UserName x) (UserName y)     = UserName $ min (Text.toCaseFold x) (Text.toCaseFold y)

newtype UserPassId = UserPassId { unUserPassId :: Integer }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''UserPassId)

succUserPassId :: UserPassId -> UserPassId
succUserPassId (UserPassId i) = UserPassId (succ i)

data UserPass
    = UserPass { upName     :: UserName
               , upPassword :: HashedPass
               , upId       :: UserPassId
               }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''UserPass)

$(inferIxSet "UserPasses" ''UserPass 'noCalcs [''UserName, ''HashedPass, ''AuthId, ''UserPassId])

-- * Identifier

$(deriveSafeCopy 1 'base ''Identifier)

-- * AuthMap

newtype FacebookId_001 = FacebookId_001 { unFacebookId_001 :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)

newtype FacebookId_002 = FacebookId_002 { unFacebookId_002 :: B.ByteString }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 2 'extension ''FacebookId_002)

instance Migrate FacebookId_002 where
    type MigrateFrom FacebookId_002 = FacebookId_001
    migrate (FacebookId_001 fid) = FacebookId_002 (Text.encodeUtf8 fid)

deriving instance Data Id
$(deriveSafeCopy 0 'base ''Id)

newtype FacebookId = FacebookId { unFacebookId :: UserId }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 3 'extension ''FacebookId)

instance Migrate FacebookId where
    type MigrateFrom FacebookId = FacebookId_002
    migrate (FacebookId_002 fid) = FacebookId (Id $ Text.decodeUtf8 fid)

data AuthMethod_v1
    = AuthIdentifier_v1 { amIdentifier_v1 :: Identifier
                     }
    | AuthUserPassId_v1 { amUserPassId_v1 :: UserPassId
                     }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 1 'base ''AuthMethod_v1)

data AuthMethod
    = AuthIdentifier { amIdentifier :: Identifier
                     }
    | AuthUserPassId { amUserPassId :: UserPassId
                     }
    | AuthFacebook   { amFacebookId :: FacebookId
                     }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 2 'extension ''AuthMethod)

instance Migrate AuthMethod where
    type MigrateFrom AuthMethod = AuthMethod_v1
    migrate (AuthIdentifier_v1 ident) = AuthIdentifier ident
    migrate (AuthUserPassId_v1 up)    = AuthUserPassId up


-- | This links an authentication method (such as on OpenId 'Identifier', a 'FacebookId', or 'UserPassId') to an 'AuthId'.
data AuthMap
    = AuthMap { amMethod :: AuthMethod
              , amAuthId :: AuthId
              }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 1 'base ''AuthMap)

$(inferIxSet "AuthMaps" ''AuthMap 'noCalcs [''AuthId, ''AuthMethod, ''Identifier, ''UserPassId, ''FacebookId])

-- * AuthToken

data AuthToken_001
    = AuthToken_001 { tokenString_001     :: String
                    , tokenExpires_001    :: UTCTime
                    , tokenAuthId_001     :: Maybe AuthId
                    , tokenAuthMethod_001 :: AuthMethod
                    }
      deriving (Eq, Ord, Data, Show, Typeable)
$(deriveSafeCopy 1 'base ''AuthToken_001)


data AuthToken
    = AuthToken { tokenString     :: String
                , tokenExpires    :: UTCTime
                , tokenLifetime   :: Int
                , tokenAuthId     :: Maybe AuthId
                , tokenAuthMethod :: AuthMethod
                }
      deriving (Eq, Ord, Data, Show, Typeable)
$(deriveSafeCopy 2 'extension ''AuthToken)

instance Migrate AuthToken where
    type MigrateFrom AuthToken = AuthToken_001
    migrate (AuthToken_001 ts te tid tam) =
        (AuthToken ts te 3600 tid tam)

instance Indexable AuthToken where
    empty = ixSet [ ixFun $ (:[]) . tokenString
                  , ixFun $ (:[]) . tokenAuthId
                  ]

type AuthTokens = IxSet AuthToken

-- * AuthState

-- how to we remove expired AuthTokens?
--
-- Since the user might be logged in a several machines they might have several auth tokens. So we can not just expire the old ones everytime they log in.
--
-- Basically we can expired them on: logout and time
--
-- time is tricky because we do not really want to do a db update everytime they access the site
data AuthState_1
    = AuthState_1 { userPasses_1      :: UserPasses
                , nextUserPassId_1  :: UserPassId
                , authMaps_1        :: AuthMaps
                , nextAuthId_1      :: AuthId
                , authTokens_1      :: AuthTokens
                }
      deriving (Data, Eq, Show, Typeable)
$(deriveSafeCopy 1 'base ''AuthState_1)

data AuthState
    = AuthState { userPasses      :: UserPasses
                , nextUserPassId  :: UserPassId
                , authMaps        :: AuthMaps
                , nextAuthId      :: AuthId
                , authTokens      :: AuthTokens
                , defaultSessionTimeout  :: Int
                }
      deriving (Data, Eq, Show, Typeable)
$(deriveSafeCopy 2 'extension ''AuthState)

instance Migrate AuthState where
    type MigrateFrom AuthState = AuthState_1
    migrate (AuthState_1 up nup am nai at) =
        (AuthState up nup am nai at (60*60))

-- | a reasonable initial 'AuthState'
initialAuthState :: AuthState
initialAuthState =
    AuthState { userPasses      = IxSet.empty
              , nextUserPassId  = UserPassId 1
              , authMaps        = IxSet.empty
              , authTokens      = IxSet.empty
              , nextAuthId      = AuthId 1
              , defaultSessionTimeout  = 60*60
              }

-- ** UserPass

modifyUserPass :: UserPassId -> (UserPass -> UserPass) -> Update AuthState (Maybe UserPassError)
modifyUserPass upid fn =
    do as@(AuthState {..}) <- get
       case getOne $ userPasses @= upid of
         Nothing -> return (Just $ InvalidUserPassId upid)
         (Just userPass) ->
             do let userPass' = fn userPass
                put as { userPasses = IxSet.updateIx upid userPass' userPasses }
                return Nothing

-- | errors that can occur when working with 'UserPass'
data UserPassError
    = UsernameInUse UserName
    | InvalidUserPassId UserPassId
    | InvalidUserName UserName
    | InvalidPassword
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''UserPassError)

-- | return a user-friendly error message string for an 'AddAuthError'
userPassErrorString :: UserPassError -> String
userPassErrorString (UsernameInUse (UserName txt))     = "Username already in use: " ++ Text.unpack txt
userPassErrorString (InvalidUserPassId (UserPassId i)) = "Invalid UserPassId " ++ show i
userPassErrorString (InvalidUserName (UserName name))  = "Invalid username " ++ Text.unpack name
userPassErrorString InvalidPassword                    = "Invalid password"

-- | creates a new 'UserPass'
createUserPass :: UserName      -- ^ desired username
             -> HashedPass   -- ^ hashed password
             -> Update AuthState (Either UserPassError UserPass)
createUserPass name hashedPass =
    do as@(AuthState{..}) <- get
       if not (IxSet.null $ userPasses @= name)
         then return (Left (UsernameInUse name))
         else do let userPass = UserPass { upName     = name
                                         , upPassword = hashedPass
                                         , upId       = nextUserPassId
                                         }
                 put $ as { userPasses     = IxSet.insert userPass userPasses
                          , nextUserPassId = succUserPassId nextUserPassId
                          }
                 return (Right userPass)

-- | change the 'UserName' associated with a 'UserPassId'
-- this will break password salting...
setUserName :: UserPassId -> Text -> Update AuthState (Maybe UserPassError)
setUserName upid name =
    do as <- get
       if nameAvailable (userPasses as)
         then case getOne $ (userPasses as) @= upid of
                (Just userPass) ->
                    do put $ as { userPasses = IxSet.updateIx upid (userPass { upName = UserName name }) (userPasses as) }
                       return Nothing
                Nothing -> return (Just $ InvalidUserPassId upid)
         else return (Just $ UsernameInUse (UserName name))
    where
      nameAvailable userPasses =
          case IxSet.toList (userPasses @= (UserName name)) of
            [] -> True
            [a] | (upId a == upid) -> True
            _ -> False

-- | hash a password string
mkHashedPass :: Text          -- ^ password in plain text
             -> IO HashedPass -- ^ salted and hashed
mkHashedPass pass = HashedPass <$> makePassword (Text.encodeUtf8 pass) 12

-- | verify a password
verifyHashedPass :: Text       -- ^ password in plain text
                 -> HashedPass -- ^ hashed version of password
                 -> Bool
verifyHashedPass passwd (HashedPass hashedPass) =
    verifyPassword (Text.encodeUtf8 passwd) hashedPass

-- | change the password for the give 'UserPassId'
setPassword :: UserPassId -> HashedPass -> Update AuthState (Maybe UserPassError)
setPassword upid hashedPass =
    modifyUserPass upid $ \userPass ->
        userPass { upPassword = hashedPass }

checkUserPass :: Text -> Text -> Query AuthState (Either UserPassError UserPassId)
checkUserPass username password =
    do as@(AuthState{..}) <- ask
       case IxSet.getOne $ userPasses @= (UserName username) of
         Nothing -> return (Left $ InvalidUserName (UserName username))
         (Just userPass)
             | verifyHashedPass password (upPassword userPass) ->
                 do return (Right (upId userPass))
             | otherwise -> return (Left InvalidPassword)

askUserPass :: UserPassId -> Query AuthState (Maybe UserPass)
askUserPass uid =
    do as@(AuthState{..}) <-ask
       return $ getOne $ userPasses @= uid

-- ** AuthMap

addAuthMethod :: AuthMethod -> AuthId -> Update AuthState ()
addAuthMethod authMethod authid =
    do as@(AuthState{..}) <- get
       put $ as { authMaps = IxSet.insert (AuthMap authMethod authid) authMaps }

newAuthMethod :: AuthMethod -> Update AuthState AuthId
newAuthMethod authMethod =
    do as@(AuthState{..}) <- get
       put $ as { authMaps = IxSet.insert (AuthMap authMethod nextAuthId) authMaps
                , nextAuthId = succAuthId nextAuthId
                }
       return nextAuthId

removeAuthIdentifier :: Identifier -> AuthId -> Update AuthState ()
removeAuthIdentifier identifier authid =
    do as@(AuthState{..}) <- get
       put $ as { authMaps = IxSet.delete (AuthMap (AuthIdentifier identifier) authid) authMaps }

identifierAuthIds :: Identifier -> Query AuthState (Set AuthId)
identifierAuthIds identifier =
    do as@(AuthState{..}) <- ask
       return $ Set.map amAuthId $ IxSet.toSet $ authMaps @= identifier

facebookAuthIds :: FacebookId -> Query AuthState (Set AuthId)
facebookAuthIds facebookId =
    do as@(AuthState{..}) <- ask
       return $ Set.map amAuthId $ IxSet.toSet $ authMaps @= facebookId


addAuthUserPassId :: UserPassId -> AuthId -> Update AuthState ()
addAuthUserPassId upid authid =
    do as@(AuthState{..}) <- get
       put $ as { authMaps = IxSet.insert (AuthMap (AuthUserPassId upid) authid) authMaps }

removeAuthUserPassId :: UserPassId -> AuthId -> Update AuthState ()
removeAuthUserPassId upid authid =
    do as@(AuthState{..}) <- get
       put $ as { authMaps = IxSet.delete (AuthMap (AuthUserPassId upid) authid) authMaps }

userPassIdAuthIds :: UserPassId -> Query AuthState (Set AuthId)
userPassIdAuthIds upid =
    do as@(AuthState{..}) <- ask
       return $ Set.map amAuthId $ IxSet.toSet $ authMaps @= upid

-- * Default timeout

setDefaultSessionTimeout :: Int -- ^ default timout in seconds (should be >= 180)
               -> Update AuthState ()
setDefaultSessionTimeout newTimeout =
    modify $ \as@AuthState{..} -> as { defaultSessionTimeout = newTimeout }

getDefaultSessionTimeout :: Query AuthState Int
getDefaultSessionTimeout =
    defaultSessionTimeout <$> ask

-- * AuthToken


addAuthToken :: AuthToken -> Update AuthState ()
addAuthToken authToken =
    do as@AuthState{..} <- get
       put (as { authTokens = IxSet.insert authToken authTokens })

-- | look up the 'AuthToken' associated with the 'String'
askAuthToken :: String  -- ^ token string (used in the cookie)
             -> Query AuthState (Maybe AuthToken)
askAuthToken tokenStr =
    do as@AuthState{..} <- ask
       return $ getOne $ authTokens @= tokenStr

updateAuthToken :: AuthToken -> Update AuthState ()
updateAuthToken authToken =
    do as@AuthState{..} <- get
       put (as { authTokens = IxSet.updateIx (tokenString authToken) authToken authTokens })

deleteAuthToken :: String -> Update AuthState ()
deleteAuthToken tokenStr =
    do as@AuthState{..} <- get
       put (as { authTokens = IxSet.deleteIx tokenStr authTokens })

purgeExpiredTokens :: UTCTime
                   -> Update AuthState ()
purgeExpiredTokens now =
    do as@AuthState{..} <- get
       let authTokens' = IxSet.fromList $ filter (\at -> (tokenExpires at) > now) (IxSet.toList authTokens)
       put as { authTokens = authTokens'}

-- | deprecated
--
-- this function is deprecated because it is not possible to check if the session has expired
authTokenAuthId :: String -> Query AuthState (Maybe AuthId)
authTokenAuthId tokenString =
    do as@(AuthState{..}) <- ask
       case getOne $ authTokens @= tokenString of
         Nothing          -> return Nothing
         (Just authToken) -> return $ (tokenAuthId authToken)

-- | generate a new, unused 'AuthId'
genAuthId :: Update AuthState AuthId
genAuthId =
    do as@(AuthState {..}) <- get
       put (as { nextAuthId = succAuthId nextAuthId })
       return nextAuthId

askAuthState :: Query AuthState AuthState
askAuthState = ask

$(makeAcidic ''AuthState [ 'askUserPass
                         , 'checkUserPass
                         , 'createUserPass
                         , 'setUserName
                         , 'setPassword
                         , 'addAuthToken
                         , 'askAuthToken
                         , 'updateAuthToken
                         , 'deleteAuthToken
                         , 'purgeExpiredTokens
                         , 'authTokenAuthId
                         , 'genAuthId
                         , 'addAuthMethod
                         , 'newAuthMethod
                         , 'removeAuthIdentifier
                         , 'identifierAuthIds
                         , 'facebookAuthIds
                         , 'addAuthUserPassId
                         , 'removeAuthUserPassId
                         , 'userPassIdAuthIds
                         , 'askAuthState
                         , 'setDefaultSessionTimeout
                         , 'getDefaultSessionTimeout
                        ])

-- * happstack-server level stuff

-- TODO:
--  - expireAuthTokens
--  - tickleAuthToken

-- | generate an new authentication token
--
genAuthToken :: (MonadIO m) => Maybe AuthId -> AuthMethod -> Int -> m AuthToken
genAuthToken aid authMethod lifetime =
    do random <- liftIO $ B.unpack . exportSalt <$> genSaltIO -- the docs promise that the salt will be base64, so 'B.unpack' should be safe
       now <- liftIO $ getCurrentTime
       let expires = addUTCTime (fromIntegral lifetime) now
           prefix = case aid of
                      Nothing  -> "0"
                      (Just a) -> show (unAuthId a)
       return $ AuthToken { tokenString     = prefix ++ random
                          , tokenExpires    = expires
                          , tokenLifetime   = lifetime
                          , tokenAuthId     = aid
                          , tokenAuthMethod = authMethod
                          }

-- Also calls 'PurgeExpiredTokens'
addAuthCookie :: (Happstack m) =>
                 AcidState AuthState
              -> Maybe AuthId
              -> AuthMethod
              -> m ()
addAuthCookie acidH aid authMethod =
    do lifetime <- query' acidH GetDefaultSessionTimeout
       authToken <- genAuthToken aid authMethod lifetime
       now <- liftIO $ getCurrentTime
       update' acidH (PurgeExpiredTokens now)
       update' acidH (AddAuthToken authToken)
       s <- rqSecure <$> askRq
       addCookie Session ((mkCookie "authToken" (tokenString authToken)) { secure = s })
       return ()

deleteAuthCookie :: (Happstack m, Alternative m) => AcidState AuthState -> m ()
deleteAuthCookie acidH =
    do mTokenStr <- optional $ lookCookieValue "authToken"
       case mTokenStr of
         Nothing         -> return ()
         (Just tokenStr) ->
             do expireCookie "authToken"
                update' acidH (DeleteAuthToken tokenStr)
{-
getAuthCookie :: (Alternative m, Happstack m) =>
                 AcidState AuthState
              -> m (Maybe AuthToken)
getAuthCookie acidH =
    do mTokenStr <- optional $ lookCookieValue "authToken"
-}
getAuthToken :: (Alternative m, Happstack m) => AcidState AuthState -> m (Maybe AuthToken)
getAuthToken acidH =
    do mTokenStr <- optional $ lookCookieValue "authToken"
       case mTokenStr of
         Nothing         -> return Nothing
         (Just tokenStr) ->
           do mAuthToken <- query' acidH (AskAuthToken tokenStr)
              case mAuthToken of
                Nothing -> return Nothing
                (Just authToken) ->
                    do now <- liftIO $ getCurrentTime
                       if now > (tokenExpires authToken)
                         then do expireCookie "authToken"
                                 update' acidH (DeleteAuthToken tokenStr)
                                 return Nothing
                         else if (diffUTCTime (addUTCTime (fromIntegral $ tokenLifetime authToken) now) (tokenExpires authToken)) > 60
                                then do let newAuthToken = authToken { tokenExpires = addUTCTime (fromIntegral $ tokenLifetime authToken) now }
                                        update' acidH (UpdateAuthToken newAuthToken)
                                        return (Just newAuthToken)
                                else return (Just authToken)


getAuthId :: (Alternative m, Happstack m) => AcidState AuthState -> m (Maybe AuthId)
getAuthId acidH =
    do mAuthToken <- getAuthToken acidH
       case mAuthToken of
         Nothing -> return Nothing
         (Just authToken) ->
             return $ (tokenAuthId authToken)
