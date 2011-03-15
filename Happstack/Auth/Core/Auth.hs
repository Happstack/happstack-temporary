{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeSynonymInstances, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators, RecordWildCards
    #-}
module Happstack.Auth.Core.Auth
    ( UserPass(..)
    , UserName(..)
    , UserPassError(..)
    , userPassErrorString
    , SetUserName(..)
    , AuthState(..)
    , AuthToken(..)
    , AuthId(..)
    , AuthMethod(..)
    , mkHashedPass
    , genAuthToken
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
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Data (Data)
import Data.Map (Map)
import qualified Data.Map   as Map
import           Data.Set   (Set)
import qualified Data.Set   as Set
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime,)
import qualified Data.Text  as Text
import qualified Data.Text.Encoding  as Text
import           Data.Text  (Text)
import Happstack.Data
import Happstack.State
import qualified Happstack.Data.IxSet as IxSet
import           Happstack.Data.IxSet (IxSet, inferIxSet, noCalcs)
import Happstack.Data.IxSet ((@=), getOne, updateIx)
import Web.Authenticate.OpenId
import Web.Routes
import Happstack.Server

newtype AuthId = AuthId { unAuthId :: Integer }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
instance Version AuthId
$(deriveSerialize ''AuthId)
$(deriveNewData [''AuthId])

instance PathInfo AuthId where
    toPathSegments (AuthId i) = toPathSegments i
    fromPathSegments = AuthId <$> fromPathSegments

succAuthId :: AuthId -> AuthId
succAuthId (AuthId i) = AuthId (succ i)

-- * UserPass

newtype HashedPass = HashedPass ByteString
    deriving (Eq, Ord, Read, Show, Data, Typeable)
instance Version HashedPass
$(deriveSerialize ''HashedPass)

newtype UserName = UserName { unUserName :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
instance Version UserName
$(deriveSerialize ''UserName)

newtype UserPassId = UserPassId { unUserPassId :: Integer }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
instance Version UserPassId
$(deriveSerialize ''UserPassId)
$(deriveNewData [''UserPassId])

succUserPassId :: UserPassId -> UserPassId
succUserPassId (UserPassId i) = UserPassId (succ i)

data UserPass
    = UserPass { upName     :: UserName
               , upPassword :: HashedPass
               , upId       :: UserPassId
               }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Version UserPass
$(deriveSerialize ''UserPass)

$(inferIxSet "UserPasses" ''UserPass 'noCalcs [''UserName, ''HashedPass, ''AuthId, ''UserPassId])

-- * Identifier

instance Version Identifier
$(deriveSerialize ''Identifier)
$(deriveNewData [''Identifier])

-- * AuthMap

data AuthMethod
    = AuthIdentifier { amIdentifier :: Identifier
                     }
    | AuthUserPassId { amUserPassId :: UserPassId
                     }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Version AuthMethod
$(deriveSerialize ''AuthMethod)


data AuthMap 
    = AuthMap { amMethod :: AuthMethod
              , amAuthId :: AuthId
              }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Version AuthMap
$(deriveSerialize ''AuthMap)

$(inferIxSet "AuthMaps" ''AuthMap 'noCalcs [''AuthId, ''AuthMethod, ''Identifier, ''UserPassId])

-- * AuthToken

data AuthToken 
    = AuthToken { tokenString     :: String
                , tokenExpires    :: UTCTime
                , tokenAuthId     :: Maybe AuthId
                , tokenAuthMethod :: AuthMethod
                }
      deriving (Eq, Ord, Data, Show, Typeable)
$(deriveSerialize ''AuthToken)
instance Version AuthToken

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
--                , preferedIdentifierAuthId :: Map Identifier AuthId
                , authTokens      :: AuthTokens
                }
      deriving (Data, Eq, Show, Typeable)
$(deriveSerialize ''AuthState)
instance Version AuthState

instance Component AuthState where
    type Dependencies AuthState = End
    initialValue = AuthState { userPasses      = IxSet.empty
                             , nextUserPassId  = UserPassId 1
                             , authMaps        = IxSet.empty
                             , authTokens      = IxSet.empty
--                             , preferedIdentifierAuthId = Map.empty
                             , nextAuthId      = AuthId 1
                             }

-- ** UserPass

modifyUserPass :: UserPassId -> (UserPass -> UserPass) -> Update AuthState (Maybe UserPassError)
modifyUserPass upid fn =
    do as@(AuthState {..}) <- ask
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
instance Version UserPassError
$(deriveSerialize ''UserPassError)

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
    do as <- ask
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

checkUserPass :: Text -> Text -> Query AuthState (Either UserPassError (Set AuthId))
checkUserPass username password =
    do as@(AuthState{..}) <- ask
       case IxSet.getOne $ userPasses @= (UserName username) of
         Nothing -> return (Left $ InvalidUserName (UserName username))
         (Just userPass)
             | verifyHashedPass password (upPassword userPass) -> 
                 do return (Right Set.empty)
             | otherwise -> return (Left InvalidPassword)

-- ** AuthMap

addAuthMethod :: AuthMethod -> AuthId -> Update AuthState ()
addAuthMethod authMethod authid =
    do as@(AuthState{..}) <- ask
       put $ as { authMaps = IxSet.insert (AuthMap authMethod authid) authMaps }

newAuthMethod :: AuthMethod -> Update AuthState AuthId
newAuthMethod authMethod =
    do as@(AuthState{..}) <- ask
       put $ as { authMaps = IxSet.insert (AuthMap authMethod nextAuthId) authMaps 
                , nextAuthId = succAuthId nextAuthId
                }
       return nextAuthId

removeAuthIdentifier :: Identifier -> AuthId -> Update AuthState ()
removeAuthIdentifier identifier authid =
    do as@(AuthState{..}) <- ask
       put $ as { authMaps = IxSet.delete (AuthMap (AuthIdentifier identifier) authid) authMaps }

identifierAuthIds :: Identifier -> Query AuthState (Set AuthId)
identifierAuthIds identifier =
    do as@(AuthState{..}) <- ask
       return $ Set.map amAuthId $ IxSet.toSet $ authMaps @= identifier
    
addAuthUserPassId :: UserPassId -> AuthId -> Update AuthState ()
addAuthUserPassId upid authid =
    do as@(AuthState{..}) <- ask
       put $ as { authMaps = IxSet.insert (AuthMap (AuthUserPassId upid) authid) authMaps }

removeAuthUserPassId :: UserPassId -> AuthId -> Update AuthState ()
removeAuthUserPassId upid authid =
    do as@(AuthState{..}) <- ask
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
    do random <- liftIO $ B.unpack . exportSalt <$> genSaltIO -- ^ the docs promise that the salt will be base64, so 'B.unpack' should be safe
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

$(mkMethods ''AuthState [ 'checkUserPass
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
                        , 'addAuthUserPassId
                        , 'removeAuthUserPassId
                        , 'userPassIdAuthIds
                        , 'askAuthState
                        ])

-- * happstack-server level stuff

addAuthCookie :: (Happstack m) => Maybe AuthId -> AuthMethod -> m ()
addAuthCookie aid authMethod =
    do authToken <- genAuthToken aid authMethod (60*60) 
       update (AddAuthToken authToken)
       addCookie Session (mkCookie "authToken" (tokenString authToken))
       return ()

deleteAuthCookie :: (Happstack m, Alternative m) => m ()
deleteAuthCookie =
    do mTokenStr <- optional $ lookCookieValue "authToken"
       case mTokenStr of
         Nothing         -> return ()
         (Just tokenStr) -> 
             do expireCookie "authToken"
                update (DeleteAuthToken tokenStr)

getAuthToken :: (Alternative m, Happstack m) => m (Maybe AuthToken)
getAuthToken =
    do mTokenStr <- optional $ lookCookieValue "authToken"
       case mTokenStr of
         Nothing -> return Nothing
         (Just tokenStr) ->
             query (AskAuthToken tokenStr)
             
getAuthId :: (Alternative m, Happstack m) => m (Maybe AuthId)
getAuthId =
    do mTokenStr <- optional $ lookCookieValue "authToken"
       case mTokenStr of
         Nothing         -> return Nothing
         (Just tokenStr) -> query (AuthTokenAuthId tokenStr)
