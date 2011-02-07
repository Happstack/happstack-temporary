{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Pages.Login
    (googlePage)
    where
{-
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
import Control.Applicative        ((<*>), (<$>), (<*))
import Control.Monad              (replicateM)
import Control.Monad.Trans        (MonadIO(liftIO))
import Data.Maybe                 (mapMaybe)
import qualified Data.Set         as Set
import Data.Text                  (Text)
import Data.Time.Clock            (getCurrentTime)
import Happstack.Server           (CookieLife(Session), Response, ServerMonad(..), FilterMonad(..), Happstack, addCookie, internalServerError, lookPairs, mkCookie, seeOther, toResponse)
import Happstack.State            (query, update)
import HSP                        (Attr(..), EmbedAsAttr(..), EmbedAsChild(..), XMLGenT, genElement)
-- import Pages.AppTemplate          (appTemplate, appTemplate')
-- import Pages.InternalServerError  (internalServerErrorPage)
-- import State.Profile              (AddIdentifier(..), AskProfileByAuthId(..), AskProfileByIdentifier(..), CreateProfile(..), SetAuthToken(..))
-- import Pages.FormPart             (formPart, fieldset, ol, li)
import State.Auth                 -- (AuthId(..), CheckAuth(..), GenUserId(..), genAuthToken)
-- import StoryPrompts               (StoryForm, StoryPrompts, seeOtherURL)
-- import StoryPromptsURL            (StoryPromptsURL(..), AuthURL(..), OpenIdProvider(..))
import System.Random              (randomRIO)
import Text.Digestive
import Text.Digestive.HSP.Html4
-- import Types                      (AuthToken(..), Profile(userId))
import Web.Authenticate.OpenId    (Identifier, authenticate, getForwardUrlRealm)
import Web.Authenticate.OpenId.Providers (google, yahoo, livejournal, myspace)
import Web.Routes                 (ShowURL, showURL, URL)

googlePage :: (Happstack m, ShowURL m, URL m ~ AuthURL) => Maybe String -> m Response
googlePage realm =
    do openIdUrl <- showURL A_OpenId
       gotoURL <- liftIO $ getForwardUrlRealm google openIdUrl realm []
       seeOther gotoURL (toResponse gotoURL)

openIdPage :: (Happstack m) => m Identifier
openIdPage =
    do pairs'      <- lookPairs
       let pairs = mapMaybe (\(k, ev) -> case ev of (Left _) -> Nothing ; (Right v) -> Just (k, v)) pairs'
       liftIO $ authenticate pairs

identifierToAuthId :: Identifier -> m Response
identifierToAuthId identifier =
    do authIds <- query (IdentifierAuthId identifier)
       case Set.size userIds of
         0 -> do uid <- createNewProfile authId
                 addAuthCookie uid
                 return $ toResponse $ "logged in as " ++ show uid

         
       

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