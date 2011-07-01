{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeSynonymInstances, DeriveDataTypeable,
    FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators, RecordWildCards
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
    , AuthTokenAuthId(..)
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

import Control.Applicative  (Alternative, (<$>), optional)
import Control.Monad        (replicateM)
import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)
import Control.Monad.Trans  (MonadIO(..))
import Crypto.PasswordStore
import Data.Acid
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Data (Data, Typeable)
import Data.Map (Map)
import qualified Data.Map   as Map
import Data.SafeCopy -- (base, deriveSafeCopy)
import           Data.Set   (Set)
import qualified Data.Set   as Set
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime,)
import qualified Data.Text  as Text
import qualified Data.Text.Encoding  as Text
import           Data.Text  (Text)
import qualified Happstack.Data.IxSet as IxSet
import           Happstack.Data.IxSet (IxSet, inferIxSet, noCalcs)
import Happstack.Data.IxSet ((@=), getOne, updateIx)
import Web.Authenticate.OpenId
import Web.Routes
import Happstack.Server

instance (SafeCopy a, Ord a, Typeable a, IxSet.Indexable a) => SafeCopy (IxSet a) where
    putCopy ixSet = contain $ safePut (IxSet.toList ixSet)
    getCopy = contain $ IxSet.fromList <$> safeGet


newtype AuthId = AuthId { unAuthId :: Integer }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''AuthId)
-- instance Version AuthId
-- $(deriveSerialize ''AuthId)
-- $(deriveNewData [''AuthId])

instance PathInfo AuthId where
    toPathSegments (AuthId i) = toPathSegments i
    fromPathSegments = AuthId <$> fromPathSegments

succAuthId :: AuthId -> AuthId
succAuthId (AuthId i) = AuthId (succ i)

-- * UserPass

newtype HashedPass = HashedPass ByteString
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''HashedPass)
-- instance Version HashedPass
-- $(deriveSerialize ''HashedPass)

newtype UserName = UserName { unUserName :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''UserName)
-- instance Version UserName
-- $(deriveSerialize ''UserName)

newtype UserPassId = UserPassId { unUserPassId :: Integer }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''UserPassId)
-- instance Version UserPassId
-- $(deriveSerialize ''UserPassId)
-- $(deriveNewData [''UserPassId])

succUserPassId :: UserPassId -> UserPassId
succUserPassId (UserPassId i) = UserPassId (succ i)

data UserPass
    = UserPass { upName     :: UserName
               , upPassword :: HashedPass
               , upId       :: UserPassId
               }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''UserPass)
-- instance Version UserPass
-- $(deriveSerialize ''UserPass)

$(inferIxSet "UserPasses" ''UserPass 'noCalcs [''UserName, ''HashedPass, ''AuthId, ''UserPassId])

-- * Identifier

$(deriveSafeCopy 1 'base ''Identifier)
-- instance Version Identifier
-- $(deriveSerialize ''Identifier)
-- $(deriveNewData [''Identifier])

-- * AuthMap

data AuthMethod_v1
    = AuthIdentifier_v1 { amIdentifier_v1 :: Identifier
                     }
    | AuthUserPassId_v1 { amUserPassId_v1 :: UserPassId
                     }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 1 'base ''AuthMethod_v1)

-- instance Version AuthMethod_v1
-- $(deriveSerialize ''AuthMethod_v1)

newtype FacebookId = FacebookId { unFacebookId :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)

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


data AuthMap 
    = AuthMap { amMethod :: AuthMethod
              , amAuthId :: AuthId
              }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 1 'base ''AuthMap)
-- instance Version AuthMap
-- $(deriveSerialize ''AuthMap)

$(inferIxSet "AuthMaps" ''AuthMap 'noCalcs [''AuthId, ''AuthMethod, ''Identifier, ''UserPassId, ''FacebookId])

-- * AuthToken

data AuthToken 
    = AuthToken { tokenString     :: String
                , tokenExpires    :: UTCTime
                , tokenAuthId     :: Maybe AuthId
                , tokenAuthMethod :: AuthMethod
                }
      deriving (Eq, Ord, Data, Show, Typeable)
$(deriveSafeCopy 1 'base ''AuthToken)
-- $(deriveSerialize ''AuthToken)
-- instance Version AuthToken

$(inferIxSet "AuthTokens" ''AuthToken 'noCalcs [''String, ''AuthId])



-- * AuthState

-- how to we remove expired AuthTokens?
-- 
-- Since the user might be logged in a several machines they might have several auth tokens. So we can not just expire the old ones everytime they log in.
-- 
-- Basically we can expired them on: logout and time
--
-- time is tricky because we do not really want to do a db update everytime they access the site
data AuthState 
    = AuthState { userPasses      :: UserPasses
                , nextUserPassId  :: UserPassId
                , authMaps        :: AuthMaps
                , nextAuthId      :: AuthId
                , authTokens      :: AuthTokens
                }
      deriving (Data, Eq, Show, Typeable)
$(deriveSafeCopy 1 'base ''AuthState)
-- $(deriveSerialize ''AuthState)
-- instance Version AuthState

-- | a reasonable initial 'AuthState'
initialAuthState :: AuthState
initialAuthState =
    AuthState { userPasses      = IxSet.empty
              , nextUserPassId  = UserPassId 1
              , authMaps        = IxSet.empty
              , authTokens      = IxSet.empty
              , nextAuthId      = AuthId 1
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
-- instance Version UserPassError
-- $(deriveSerialize ''UserPassError)

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

authTokenAuthId :: String -> Query AuthState (Maybe AuthId)
authTokenAuthId tokenString =
    do as@(AuthState{..}) <- ask
       case getOne $ authTokens @= tokenString of
         Nothing          -> return Nothing
         (Just authToken) -> return $ (tokenAuthId authToken)

-- TODO: 
--  - expireAuthTokens
--  - tickleAuthToken  

-- | generate an new authentication token
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
                          , tokenAuthId     = aid
                          , tokenAuthMethod = authMethod
                          }

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
                        ])

-- * happstack-server level stuff

addAuthCookie :: (Happstack m) => AcidState AuthState -> Maybe AuthId -> AuthMethod -> m ()
addAuthCookie acidH aid authMethod =
    do authToken <- genAuthToken aid authMethod (60*60) 
       update' acidH (AddAuthToken authToken)
       addCookie Session (mkCookie "authToken" (tokenString authToken))
       return ()

deleteAuthCookie :: (Happstack m, Alternative m) => AcidState AuthState -> m ()
deleteAuthCookie acidH =
    do mTokenStr <- optional $ lookCookieValue "authToken"
       case mTokenStr of
         Nothing         -> return ()
         (Just tokenStr) -> 
             do expireCookie "authToken"
                update' acidH (DeleteAuthToken tokenStr)

getAuthToken :: (Alternative m, Happstack m) => AcidState AuthState -> m (Maybe AuthToken)
getAuthToken acidH =
    do mTokenStr <- optional $ lookCookieValue "authToken"
       case mTokenStr of
         Nothing -> return Nothing
         (Just tokenStr) ->
             query' acidH (AskAuthToken tokenStr)
             
getAuthId :: (Alternative m, Happstack m) => AcidState AuthState -> m (Maybe AuthId)
getAuthId acidH =
    do mTokenStr <- optional $ lookCookieValue "authToken"
       case mTokenStr of
         Nothing         -> return Nothing
         (Just tokenStr) -> query' acidH (AuthTokenAuthId tokenStr)
