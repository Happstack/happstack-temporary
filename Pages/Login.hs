{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Pages.Login where
{-
    ( googlePage
    , openIdPage
    )
    ( genericOpenIdPage
    , googlePage
    , liveJournalPage
    , loginPage
    , localLoginPage
    , myspacePage
    , openIdPage
    , yahooPage
    ) 
-}
import AuthURL
import Control.Applicative        (Alternative, (<*>), (<$>), (<*), optional)
import Control.Monad              (replicateM)
import Control.Monad.Trans        (MonadIO(liftIO))
import Data.Maybe                 (mapMaybe)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import Data.Text                  (Text)
import Data.Time.Clock            (getCurrentTime)
import Happstack.Server           (CookieLife(Session), Response, ServerMonad(..), FilterMonad(..), Happstack, ServerPartT, addCookie, escape, internalServerError, lookCookieValue, lookPairs, mkCookie, seeOther, toResponse)
import Happstack.State            (query, update)
import HSP                        (Attr(..), EmbedAsAttr(..), EmbedAsChild(..), XMLGenT, genElement)
import State.Auth
import Pages.AppTemplate
import Profile
import ProfileURL
import Text.Digestive
import Text.Digestive.HSP.Html4
import Web.Authenticate.OpenId    (Identifier, authenticate, getForwardUrl)
import Web.Authenticate.OpenId.Providers (google, yahoo, livejournal, myspace)
import Web.Routes                 (RouteT, ShowURL, showURL, URL)
import Web.Routes.XMLGenT

-- * AuthURL stuff

googlePage :: (Happstack m, ShowURL m, URL m ~ AuthURL) => AuthMode -> Maybe String -> m Response
googlePage authMode realm =
    do openIdUrl <- showURL (A_OpenId authMode)
       gotoURL <- liftIO $ getForwardUrl google openIdUrl realm []
       seeOther gotoURL (toResponse gotoURL)              

-- this verifies the identifier
-- and sets authToken cookie
-- if the identifier was not associated with an AuthId, then a new AuthId will be created and associated with it.
openIdPage :: AuthMode -> String -> RouteT AuthURL (ServerPartT IO) Response -- (Maybe AuthId)
openIdPage LoginMode onAuthURL = 
    do identifier <- getIdentifier
       addAuthIdsCookie identifier
       seeOther onAuthURL (toResponse ())



-- this get's the identifier the openid provider provides. It is our only chance to capture the Identifier. So, before we send a Response we need to have some sort of cookie set that identifies the user. We can not just put the identifier in the cookie because we don't want some one to fake it.
getIdentifier :: (Happstack m) => m Identifier
getIdentifier =
    do pairs'      <- lookPairs
       let pairs = mapMaybe (\(k, ev) -> case ev of (Left _) -> Nothing ; (Right v) -> Just (k, v)) pairs'
       (identifier, _) <- liftIO $ authenticate pairs
       return identifier

-- calling this will log you in as 1 or more AuthIds
-- problem.. if the Identifier is not associated with any Auths, then we are in trouble, because the identifier will be 'lost'.
-- so, if there are no AuthIds associated with the identifier, we create one.
--
-- we have another problem though.. we want to allow a user to specify a prefered AuthId. But that preference needs to be linked to a specific Identifier ?
addAuthIdsCookie :: (Happstack m) => Identifier -> m (Maybe AuthId)
addAuthIdsCookie identifier =
    do authId <- 
           do authIds <- query (IdentifierAuthIds identifier)
              case Set.size authIds of
                 1 -> return $ (Just $ head $ Set.toList $ authIds)
                 n -> return $ Nothing
       addAuthCookie authId (AuthIdentifier identifier)
       return authId

-- * ProfileURL stuff

-- can we pick an AuthId with only the information in the Auth stuff? Or should that be a profile action ?

pickAuthId :: RouteT ProfileURL (ServerPartT IO) AuthId
pickAuthId =
    do (Just authToken) <- getAuthToken -- FIXME: Just 
       case tokenAuthId authToken of
         (Just authId) -> return authId
         Nothing ->
             do authIds <- query (IdentifierAuthIds (amIdentifier $ tokenAuthMethod authToken)) -- FIXME: might not be an Identifier
                case Set.size authIds of
                  0 -> do authId <- update (NewAuthMethod (tokenAuthMethod authToken))
                          update (UpdateAuthToken (authToken { tokenAuthId = Just authId }))
                          return authId
                  1 -> do let aid = head $ Set.toList authIds
                          update (UpdateAuthToken (authToken { tokenAuthId = Just aid }))
                          return aid
                  n -> escape $ authPicker authIds

authPicker :: Set AuthId -> RouteT ProfileURL (ServerPartT IO) Response
authPicker authIds =
    appTemplate "Pick An Auth" ()
                <div>
                 <ul><% mapM auth (Set.toList authIds) %></ul>
                </div>
    where
      auth authId =
          <li><a href=(P_SetAuthId authId)><% show authId %></a></li> -- FIXME: give a more informative view


-- now that we have things narrowed down to a single 'AuthId', pick which personality we want to be
pickProfile :: String -> RouteT ProfileURL (ServerPartT IO) Response
pickProfile onLoginURL =
    do aid <- pickAuthId
       mUid <- query (AuthIdUserId aid)
       case mUid of
         Nothing ->
             do profiles <- query (AuthIdProfiles aid)
                case Set.size profiles of
                  0 -> do uid <- update (CreateNewProfile (Set.singleton aid))
                          update (SetAuthIdUserId aid uid)
                          seeOther onLoginURL (toResponse onLoginURL)
                  1 -> do let profile = head $ Set.toList profiles
                          update (SetAuthIdUserId aid (userId profile))
                          seeOther onLoginURL (toResponse onLoginURL)
                  n -> do personalityPicker profiles
         (Just uid) ->
             seeOther onLoginURL (toResponse onLoginURL)

personalityPicker :: Set Profile -> RouteT ProfileURL (ServerPartT IO) Response
personalityPicker profiles =
    appTemplate "Pick A Personality" ()
                <div>
                 <ul><% mapM personality (Set.toList profiles) %></ul>
                </div>
    where
      personality profile =
          <li><a href=(P_SetPersonality (userId profile))><% nickName profile %></a></li>
                
{-
                   Nothing -> 
                       do uid <- update (CreateNewProfile (Set.singleton aid))
                          update (SetAuthIdUserId aid uid)
                          return $ toResponse $ "logged in as " ++ show uid
                   (Just uid) ->
                       do 
-}



{-
authIdPicker :: Set AuthId -> RouteT ProfileURL (ServerPartT IO) Response
authIdPicker authIds =
    appTemplate "Pick an AuthId" ()
                <div>
                 <ul><% mapM auth (Set.toList authIds) %></ul>
                </div>
    where
      auth authId =
          <li><a href=(P_Set (userId profile))><% nickName profile %></a></li>
-}
{-
data R =
       NowUser UserId
       | PickAuth (Set AuthIds)
-}

{-
data PickUser 
    = Picked UserId
    | NeedsPickin (Set UserId)
  -}            


{-
authIdsToUserId :: (Happstack m) => Set AuthIds -> m Response
authIdsToUserId authIds =
       case Set.size authIds of
         0 -> do aid <- update GenAuthId
                 uid <- update (CreateNewProfile (Set.singleton aid))
                 update (SetAuthIdUserId aid uid)
                 return $ toResponse $ "logged in as " ++ show uid
         1 -> do let aid = (head $ Set.toList authIds)
                 mUid <- query (AuthIdUserId aid)
                 case mUid of
                   Nothing ->  -- this probably should not happen ?
                       do uid <- update (CreateNewProfile (Set.singleton aid))
                          update (SetAuthIdUserId aid uid)
                          return $ toResponse $ "logged in as " ++ show uid
                   (Just uid) ->
                       do 
-}
--         n -> do 


 -- here we have multiple auth ids to pick from. We need to show the user something. But that means stopping and generating a new page. but we will lose the Identifier then.
         



{-
foo :: (Happstack m) => Identifier -> m Response
foo identifier =
    do userIds <- query (IdentifierToUserId identifier)
       case Set.size userIds of
         -- new, never before identifier
         -- create a new account, *or* link to existing acount
         0 -> do uid <- createNewProfile identifier
                 addAuthCookie uid
                 return $ toResponse $ "logged in as " ++ show uid
         1 -> do let [uid] = Set.toList userIds
                 addAuthCookie uid
                 return $ toResponse $ "logged in as " ++ show uid
         -- pick which user to log in as this will likely require
         -- information from the profile, which we do not have access
         -- to.
         -- 
         -- also, we need a way to remember the identification 
         -- normally we set an authToken
         -- but the authToken is not yet associated with anyone..
         n -> pickProfile userIds

-}
-- linkExisting 
-- 
-- to link to an existing account we need:
-- 
--  1. a way to retrieve that account
--  2. a way to prove the person is authorized




{-
--       liftIO $ putStrLn $ "openIdPage Identifier: " ++ show identifier
       mp         <- query (AskProfileByIdentifier identifier)
--       liftIO $ putStrLn $ "openIdPage mp: " ++ show mp
       authToken  <- liftIO $ AuthToken <$> (replicateM 32 $ randomRIO ('a', 'z')) -- FIXME: not a good source of randomness
--       liftIO $ putStrLn $ "openIdPage authToken: " ++ show authToken
       addCookie Session (mkCookie "authToken" (unAuthToken authToken))
       case mp of
         Nothing -> 
             do now <- liftIO $ getCurrentTime
                uid <- update (CreateProfile now (Just authToken))
                update (AddIdentifier uid identifier)
                seeOtherURL W_EditProfile
         (Just p) ->
             do r <- update $ SetAuthToken (userId p) (Just authToken)
                if r
                 then seeOtherURL (W_Profile (userId p))
                 else internalSeraverError =<< appTemplate "failure." () <p>SetAuthToken <% show (userId p, authToken) %> failed.</p>
-}

{-
loginPage :: StoryPromptsURL -> StoryPromptsURL -> StoryPrompts Response
loginPage _here _onLogin =
    do appTemplate "Login" () $
                <div>
                 <h1>Login</h1>
                 <p>Don't want to remember another username and password? Just connect using an account you already have. It is safe and secure and does not share your password or email address with us. Learn more about OpenId <a href="http://openidexplained.com/">here</a> or click on a link below to connect instantly.</p>
                 <ul>
                  <li><a href=(W_Auth (A_OpenIdProvider Google))>Login</a> with your Google Account</li>
                  <li><a href=(W_Auth (A_OpenIdProvider Yahoo))>Login</a> with your Yahoo Account</li>
                  <li><a href=(W_Auth (A_OpenIdProvider LiveJournal))>Login</a> with your Live Journal Account</li>
                  <li><a href=(W_Auth (A_OpenIdProvider Myspace))>Login</a> with your Myspace Account</li>
                  <li><a href=(W_Auth (A_OpenIdProvider Generic))>Login</a> with your OpenId Account</li>
                 </ul>
                 <p>If you still don't trust newfangled Internet technology, you can also create an account and login in the old-fashion way.</p>
                 <ul>
                  <li><a href=(W_Auth A_Local)>Login</a> with a username and password</li>
                 </ul>
                </div>

localLoginPage :: StoryPromptsURL -> StoryPromptsURL -> StoryPrompts Response
localLoginPage here onLogin =
    do actionURL <- showURL here
       appTemplate "Login" () $
         <div id="main">
           <h1>Login with a username and password</h1>
           <% formPart "p" actionURL handleLogin handleFailure loginForm %>
        </div>
      where 
        handleFailure errs formXML =
            toResponse <$> appTemplate' "Login" ()
               <div id="main">
                <h1>Login with a username and password</h1>
                <% formXML %>
               </div>

        handleLogin aid =
            do mp <- query (AskProfileByAuthId aid)
               case mp of
--                 Nothing  -> internalServerErrorPage "Login was valid, but does not seem to be associated with any profile."
                 (Just p) -> 
                     do authToken  <- liftIO $ AuthToken <$> (replicateM 32 $ randomRIO ('a', 'z')) -- FIXME: not a good source of randomness
                        update (SetAuthToken (userId p) (Just authToken))
                        addCookie Session (mkCookie "authToken" (unAuthToken authToken))
                        seeOtherURL onLogin
        loginForm :: StoryForm AuthId
        loginForm =
            (errors ++> (fieldset $ ol (((,) <$> (li $ label "username: " ++> inputText Nothing) <*> (li $ label "password: " ++> inputPassword) <* login) `transform` checkAuth))) <* create
        create :: StoryForm ()
        create = view [<p>or <a href=(W_Auth A_CreateAccount)>create a new account</a></p>]

        login = li $ (submit "Login") `setAttrs` [("class" := "submit")]
        checkAuth = 
            transformEitherM $ \(username, password) ->
                do r <- query (CheckAuth username password)
                   case r of 
                     Nothing -> return (Left "Invalid username or password")
                     (Just authId) -> return (Right authId)


yahooPage :: Maybe String -> StoryPromptsURL -> StoryPrompts Response
yahooPage realm _onLogin =
    do openIdUrl <- showURL (W_Auth A_OpenId)
       gotoURL <- liftIO $ getForwardUrlRealm yahoo openIdUrl realm []
       seeOther gotoURL (toResponse gotoURL)

myspacePage :: Maybe String -> StoryPromptsURL -> StoryPromptsURL -> StoryPrompts Response
myspacePage realm here _onLogin =
    do actionURL <- showURL here
       appTemplate "Login" () $
        <div id="main">
         <h1>Login using your myspace account</h1>
         <% formPart "p" actionURL (connect realm . myspace) handleFailure usernameForm %>
        </div>
      where 
        usernameForm :: StoryForm String
        usernameForm = 
            label "http://www.myspace.com/" ++> inputString Nothing <* submit "Login"

liveJournalPage :: Maybe String -> StoryPromptsURL -> StoryPromptsURL -> StoryPrompts Response
liveJournalPage realm here _onLogin =
    do actionURL <- showURL here
       appTemplate "Login" () $
        <div id="main">
         <h1>Login using your Live Journal account</h1>
         <p>Enter your livejournal account name to connect. You may be prompted to log into your livejournal account and to confirm the login.</p>
         <% formPart "p" actionURL (connect realm . livejournal) handleFailure usernameForm %>
        </div>
      where 
        usernameForm :: StoryForm String
        usernameForm = 
            label "http://" ++> inputString Nothing <++ label ".livejournal.com/" <* submit "Connect"

genericOpenIdPage :: Maybe String -> StoryPromptsURL -> StoryPromptsURL -> StoryPrompts Response
genericOpenIdPage realm here _onLogin =
    do actionURL <- showURL here
       appTemplate "Login" () $
        <div id="main">
         <h1>Connect using your OpenId account</h1>
         <% formPart "p" actionURL (connect realm) handleFailure usernameForm %>
        </div>
      where 
        usernameForm :: StoryForm String
        usernameForm = 
            label "Your OpenId url: " ++> inputString Nothing <* submit "Connect"


        
 
connect :: Maybe String -> String -> XMLGenT StoryPrompts Response
connect realm url = 
    do openIdUrl <- showURL (W_Auth A_OpenId)
       gotoURL <- liftIO $ getForwardUrlRealm url openIdUrl realm []
       seeOther gotoURL (toResponse gotoURL)
         
handleFailure errs formXML =
            toResponse <$> appTemplate' "Login" ()
               <div id="main">
                <h1>Errors</h1>
                <% errorList (map snd errs) %>
                <% formXML %>
               </div>


{-
    do openIdUrl <- showURL (W_Auth A_OpenId)
       gotoURL <- liftIO $ getForwardUrl myopenIdUrl
       seeOther gotoURL (toResponse gotoURL)
-}


openIdPage :: StoryPrompts Response
openIdPage =
    do pairs'      <- lookPairs
       let pairs = mapMaybe (\(k, ev) -> case ev of (Left _) -> Nothing ; (Right v) -> Just (k, v)) pairs'
       identifier <- liftIO $ authenticate pairs
--       liftIO $ putStrLn $ "openIdPage Identifier: " ++ show identifier
       mp         <- query (AskProfileByIdentifier identifier)
--       liftIO $ putStrLn $ "openIdPage mp: " ++ show mp
       authToken  <- liftIO $ AuthToken <$> (replicateM 32 $ randomRIO ('a', 'z')) -- FIXME: not a good source of randomness
--       liftIO $ putStrLn $ "openIdPage authToken: " ++ show authToken
       addCookie Session (mkCookie "authToken" (unAuthToken authToken))
       case mp of
         Nothing -> 
             do now <- liftIO $ getCurrentTime
                uid <- update (CreateProfile now (Just authToken))
                update (AddIdentifier uid identifier)
                seeOtherURL W_EditProfile
         (Just p) ->
             do r <- update $ SetAuthToken (userId p) (Just authToken)
                if r
                 then seeOtherURL (W_Profile (userId p))
                 else internalServerError =<< appTemplate "failure." () <p>SetAuthToken <% show (userId p, authToken) %> failed.</p>
-}