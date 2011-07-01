{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Happstack.Auth.HSP.Login where

import Control.Applicative        (Alternative, (<*>), (<$>), (<*), (*>), optional)
import Control.Monad              (replicateM, mplus)
import Control.Monad.Trans        (MonadIO(liftIO))
import Data.Acid                  (AcidState, query', update')
import Data.Maybe                 (mapMaybe)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import Data.Text                  (Text)
import qualified Data.Text        as Text
import Data.Time.Clock            (getCurrentTime)
import Happstack.Auth.Core.Auth
import Happstack.Auth.Core.AuthParts
import Happstack.Auth.Core.AuthURL
import Happstack.Auth.Core.ProfileURL
import Happstack.Auth.Core.Profile
import Happstack.Auth.Core.ProfileParts
import Happstack.Server           -- (CookieLife(Session), Response, ServerMonad(..), FilterMonad(..), Input(..), Happstack, ServerPartT, addCookie, escape, internalServerError, lookCookieValue, lookPairs, mkCookie, seeOther, toResponse, unauthorized)
import Happstack.Server.HSP.HTML  (XML)
import HSP                        (Attr(..), EmbedAsAttr(..), EmbedAsChild(..), XMLGenT(..), XMLGenerator, genElement, genEElement, unXMLGenT)
import HSP.ServerPartT()
import qualified HSX.XMLGenerator as HSX
import Happstack.Auth.HSP.FormPart
import Text.Digestive
import Text.Digestive.Forms.Happstack ()
import Text.Digestive.HSP.Html4
import Web.Authenticate.OpenId    (Identifier, authenticate, getForwardUrl)
import Web.Authenticate.OpenId.Providers (google, yahoo, livejournal, myspace)
import Web.Routes                 (RouteT, ShowURL, showURL, showURLParams, nestURL, URL)
import Web.Routes.XMLGenT



-- * AuthURL stuff
logoutPage :: (XMLGenerator m, Alternative m, Happstack m, EmbedAsAttr m (Attr String AuthURL)) => AcidState AuthState -> XMLGenT m (HSX.XML m)
logoutPage authStateH =
    do deleteAuthCookie authStateH 
       <p>You are now logged out. Click <a href=A_Login>here</a> to log in again.</p>


loginPage :: (XMLGenerator m, EmbedAsAttr m (Attr String AuthURL)) => XMLGenT m (HSX.XML m)
loginPage =
      <ol>
       <li><a href=(A_OpenIdProvider LoginMode Google)     >Login</a> with your Google Account</li>
       <li><a href=(A_OpenIdProvider LoginMode Yahoo)      >Login</a> with your Yahoo Account</li>
       <li><a href=(A_OpenIdProvider LoginMode LiveJournal)>Login</a> with your Live Journal Account</li>
       <li><a href=(A_OpenIdProvider LoginMode Myspace)    >Login</a> with your Myspace Account</li>
       <li><a href=(A_OpenIdProvider LoginMode Generic)    >Login</a> with your OpenId Account</li>
       <li><a href=A_Local                                 >Login</a> with a username and password.</li>
      </ol>


addAuthPage :: (XMLGenerator m, EmbedAsAttr m (Attr String AuthURL)) => XMLGenT m (HSX.XML m)
addAuthPage =
      <ol>
       <li><a href=(A_OpenIdProvider AddIdentifierMode Google)     >Add</a> your Google</li>
       <li><a href=(A_OpenIdProvider AddIdentifierMode Yahoo)      >Add</a> your Yahoo Account</li>
       <li><a href=(A_OpenIdProvider AddIdentifierMode LiveJournal)>Add</a> your Live Journal Account</li>
       <li><a href=(A_OpenIdProvider AddIdentifierMode Myspace)    >Add</a> your Myspace Account</li>
       <li><a href=(A_OpenIdProvider AddIdentifierMode Generic)    >Add</a> your OpenId Account</li>
      </ol>

authPicker :: (XMLGenerator m, EmbedAsAttr m (Attr String ProfileURL)) => Set AuthId -> XMLGenT m (HSX.XML m)
authPicker authIds =
    <div>
     <ul><% mapM auth (Set.toList authIds) %></ul>
    </div>
    where
      auth authId =
          <li><a href=(P_SetAuthId authId)><% show authId %></a></li> -- FIXME: give a more informative view. 

personalityPicker :: (XMLGenerator m, EmbedAsChild m Text, EmbedAsAttr m (Attr String ProfileURL)) => 
                     Set Profile 
                  -> XMLGenT m (HSX.XML m)
personalityPicker profiles =
    <div>
     <ul><% mapM personality (Set.toList profiles) %></ul>
    </div>
    where
      personality profile =
          <li><a href=(P_SetPersonality (userId profile))><% nickName profile %></a></li>


type PageTemplate x = String -> () -> (XMLGenT x (HSX.XML x)) -> XMLGenT x Response
type PageTemplate' x = String -> () -> (XMLGenT x (HSX.XML x)) -> x Response
{-
providerPage :: (Happstack m, XMLGenerator m) => (forall body. (EmbedAsChild (RouteT AuthURL m) body) => (String -> () -> body -> XMLGenT (RouteT AuthURL m) Response)) -> OpenIdProvider -> AuthURL -> AuthMode -> RouteT AuthURL m Response
-}
providerPage appTemplate provider =
    case provider of
      Google      -> googlePage
      Yahoo       -> yahooPage
      LiveJournal -> liveJournalPage appTemplate
      -- FIXME

googlePage :: (Happstack m, ShowURL m, URL m ~ AuthURL) =>
              AuthURL
           -> AuthMode
           -> m Response
googlePage _here authMode = 
    do u <- showURLParams (A_OpenId (O_Connect authMode)) [("url", google)]
       seeOther u (toResponse ())

yahooPage :: (Happstack m, ShowURL m, URL m ~ AuthURL) => 
             AuthURL
          -> AuthMode
          -> m Response
yahooPage _here authMode =
    do u <- showURLParams (A_OpenId (O_Connect authMode)) [("url", yahoo)]
       seeOther u (toResponse ())
{-
liveJournalPage :: (Happstack m, XMLGenerator m, ToMessage (HSX.XML m), EmbedAsChild m (), Alternative m, ShowURL m, URL m ~ (OpenIdURL p)) =>
                   PageTemplate m
                -> OpenIdURL p
                -> AuthMode
                -> m Response
-}
liveJournalPage appTemplate here authMode =
    do actionURL <- showURL here
       appTemplate "Login" () $
        <div id="main">
         <h1>Login using your Live Journal account</h1>
         <p>Enter your livejournal account name to connect. You may be prompted to log into your livejournal account and to confirm the login.</p>
         <% formPart "p" actionURL handleSuccess (handleFailure appTemplate) liveJournalForm %>
        </div>
      where 
--         handleSuccess :: String -> XMLGenT (RouteT (OpenIdURL p) (ServerPartT IO)) Response
        handleSuccess username =
            do u <- showURLParams (A_OpenId (O_Connect authMode)) [("url", livejournal username)]
               seeOther u (toResponse ())
{-
handleFailure :: (XMLGenerator m, Happstack m, EmbedAsChild m (), ToMessage (HSX.XML m)) =>
                 PageTemplate m
              -> [(FormRange, String)] 
              -> [XMLGenT m (HSX.XML m)]
              -> XMLGenT m Response
-}
handleFailure appTemplate errs formXML =
            XMLGenT $ appTemplate "Login" ()
               <div id="main">
                <h1>Errors</h1>
                <% errorList (map snd errs) %>
                <% formXML %>
               </div>

liveJournalForm :: (Functor v, Monad v, XMLGenerator m) => Form v [Input] String [XMLGenT m (HSX.XML m)] String
liveJournalForm = 
    label "http://" ++> inputString Nothing <++ label ".livejournal.com/" <* submit "Connect"

handleAuth ::
  (Happstack m, Alternative m) =>
     AcidState AuthState 
  -> (String  -> () -> XMLGenT (RouteT AuthURL m) XML -> RouteT AuthURL m Response)
  -> Maybe String
  -> String
  -> AuthURL
  -> RouteT AuthURL m Response
handleAuth authStateH appTemplate realm onAuthURL url =
    case url of
      A_Login           -> appTemplate "Login"    () loginPage
      A_AddAuth         -> appTemplate "Add Auth" () addAuthPage
      A_Logout          -> appTemplate "Logout"   () (logoutPage authStateH)
      A_Local           -> localLoginPage authStateH appTemplate url onAuthURL
      A_CreateAccount   -> createAccountPage authStateH appTemplate onAuthURL url
      A_ChangePassword  -> changePasswordPage authStateH appTemplate url
      (A_OpenId oidURL) -> nestURL A_OpenId $ handleOpenId authStateH realm onAuthURL oidURL
      (A_OpenIdProvider authMode provider) 
                        -> providerPage appTemplate provider url authMode


handleProfile :: (Happstack m, Alternative m) =>
                 AcidState AuthState
              -> AcidState ProfileState
              -> (String -> () -> XMLGenT (RouteT ProfileURL m) XML -> RouteT ProfileURL m Response)
              -> String
              -> ProfileURL 
              -> RouteT ProfileURL m Response
handleProfile authStateH profileStateH appTemplate postPickedURL url =
    case url of
      P_PickProfile        -> 
          do r <- pickProfile authStateH profileStateH
             case r of
               (Picked {})                -> 
                   seeOther postPickedURL (toResponse postPickedURL)

               (PickPersonality profiles) -> 
                   appTemplate "Pick Personality" () (personalityPicker profiles)

               (PickAuthId      authIds)  ->
                   appTemplate "Pick Auth" () (authPicker authIds) 

      (P_SetAuthId authId) -> 
          do b <- setAuthIdPage authStateH authId
             if b
              then seeOther "/" (toResponse "") -- FIXME: don't hardcode destination
              else unauthorized =<< 
                     appTemplate "unauthorized" ()
                        <p>Attempted to set AuthId to <% show $ unAuthId authId %>, but failed because the Identifier is not associated with that AuthId.</p>
{-
localLoginPage :: (Happstack m, Alternative m) =>
                 (forall header body. ( EmbedAsChild (RouteT AuthURL m) XML
                                      , EmbedAsChild (RouteT AuthURL m) header
                                      , EmbedAsChild (RouteT AuthURL m) body) => 
                             (String  -> header -> body -> RouteT AuthURL m Response))
              -> AuthURL
              -> String
              -> RouteT AuthURL m Response
-}
localLoginPage authStateH appTemplate here onAuthURL =
    do actionURL <- showURL here
       appTemplate "Login" () $
         <div id="main">
           <h1>Login with a username and password</h1>
           <% formPart "p" actionURL (XMLGenT . handleLogin) (handleFailure appTemplate) loginForm %>
        </div>

      where 
        handleLogin :: (Happstack m) => UserPassId -> RouteT AuthURL m Response
        handleLogin userPassId =
            do authId <- do authIds <- query' authStateH (UserPassIdAuthIds userPassId) 
                            case Set.size authIds of
                              1 -> return (Just $ head $ Set.toList $ authIds)
                              n -> return Nothing
               addAuthCookie authStateH authId (AuthUserPassId userPassId)
               seeOther onAuthURL (toResponse ())

        loginForm :: (Functor v, MonadIO v, XMLGenerator m, EmbedAsAttr m (Attr String AuthURL)) => Form v [Input] String [XMLGenT m (HSX.XML m)] UserPassId
        loginForm =
            (errors ++> (fieldset $ ol (((,) <$> (li $ label "username: " ++> inputText Nothing) <*> (li $ label "password: " ++> inputPassword) <* login) `transform` checkAuth))) <* create

        create :: (Functor v, Monad v, XMLGenerator m, EmbedAsAttr m (Attr String AuthURL)) => Form v [Input] String [XMLGenT m (HSX.XML m)] ()
        create = view [<p>or <a href=(A_CreateAccount)>create a new account</a></p>]

        login :: (Functor v, Monad v, XMLGenerator m) => Form v [Input] String [XMLGenT m (HSX.XML m)] String
        login = li $ (submit "Login") `setAttrs` [("class" := "submit")]

        checkAuth :: (MonadIO m) => Transformer m String (Text, String) UserPassId
        checkAuth = 
            transformEitherM $ \(username, password) ->
                do r <- query' authStateH (CheckUserPass username (Text.pack password))
                   case r of 
                     (Left e) -> return (Left $ userPassErrorString e)
                     (Right userPassId) -> return (Right userPassId)

        li :: a -> a
        li = id
             
        ol :: a -> a
        ol = id

        fieldset :: a -> a
        fieldset = id



-- createAccountPage :: StoryPromptsURL -> StoryPrompts Response
createAccountPage authStateH appTemplate onAuthURL here =
    do actionURL <- showURL here
       ok =<< appTemplate "Create User Account" () 
          <div id="main">
           <h1>Create an account</h1>
           <% formPart "p" actionURL handleSuccess (handleFailure appTemplate) (newAccountForm authStateH) %>
          </div>
    where
      handleSuccess (authId, userPassId) =
          do addAuthCookie authStateH (Just authId) (AuthUserPassId userPassId)
             seeOther onAuthURL (toResponse ())

newAccountForm :: (Functor v, MonadIO v, XMLGenerator m, EmbedAsAttr m (Attr String AuthURL)) => AcidState AuthState -> Form v [Input] String [XMLGenT m (HSX.XML m)] (AuthId, UserPassId)
newAccountForm authStateH =
    fieldset (errors ++> (ol $ ((,) <$> username <*> password <* submitButton)
                `transform`
                createAccount))
    where
      br :: (XMLGenerator m, Monad v) => Form v [Input] String [XMLGenT m (HSX.XML m)] ()
      br = view [<br />]
      submitButton = li $ (submit "Create Account" `setAttrs` [("class" := "submit")])
      username  = li $ ((label "username: "         ++> inputText Nothing)
                   `validate` notEmpty) <++ errors
      password1 = li $ label "password: "         ++> inputPassword
      password2 = li $ label "confirm password: " ++> inputPassword
--       password :: StoryForm String
      password = 
          errors ++> (minLengthString 6 $ 
                      (((,) <$> password1 <*> password2) `transform` samePassword))

      samePassword = 
          transformEither $ \(p1, p2) ->
              if p1 /= p2
               then (Left "Passwords do not match.")
               else (Right p1)

--       createAccount :: (MonadIO m) => Transformer m String (Text, String) UserId
      createAccount = 
          transformEitherM $ \(username, password) ->
              do passHash <- liftIO $ mkHashedPass (Text.pack password)
                 r <- update' authStateH $ CreateUserPass (UserName username) passHash
                 -- fixme: race condition
                 case r of
                   (Left e) -> return (Left (userPassErrorString e))
                   (Right userPass) -> 
                       do authId <- update' authStateH (NewAuthMethod (AuthUserPassId (upId userPass)))
                          return (Right (authId, upId userPass))


li :: a -> a
li = id
       
ol :: a -> a
ol = id

fieldset :: a -> a
fieldset = id

notEmpty :: (Monad m) => Validator m String Text
notEmpty = (check "field can not be empty") (not . Text.null)

minLengthString 0 f = f
minLengthString 1 f = errors ++> (f `validate` (check "This field can not be empty." (not . null)))
minLengthString n f = errors ++> (f `validate` (check ("This field must be at least " ++ show n ++ " characters.") (\t -> length t >= n)))


-- changePasswordPage :: StoryPromptsURL -> StoryPrompts Response
changePasswordPage authStateH appTemplate here =
    do actionURL <- showURL here
       mAuthToken <- getAuthToken authStateH
       case mAuthToken of
         Nothing -> seeOtherURL A_Login
         (Just authToken) ->
             case tokenAuthMethod authToken of
               (AuthUserPassId userPassId) ->
                   do mUserPass <- query' authStateH (AskUserPass userPassId)
                      case mUserPass of
                        Nothing ->
                            internalServerError =<< appTemplate "Invalid UserPassId" () 
                               <div id="main">
                                 <p>Invalid UserPassId <% show $ unUserPassId userPassId %></p>
                               </div>
                        (Just userPass) ->
                            ok =<< appTemplate "Change Password" () 
                                      <div id="main">
                                        <h1>Change Password for <% unUserName $ upName userPass %></h1>
                                        <% formPart "p" actionURL (XMLGenT . handleSuccess (upId userPass)) (handleFailure appTemplate) (changePasswordForm authStateH userPass) %>
                                      </div>
                   
               _ -> ok =<< appTemplate "Change Password Failure" ()
                                 <div id="main">
                                  <p>This account does not use a username and password.</p>
                                 </div>
    where
      handleSuccess userPassId passwd =
          do hashedPass <- liftIO $ mkHashedPass (Text.pack passwd)
             r <- update' authStateH (SetPassword userPassId hashedPass)
             case r of
               (Just e) -> 
                   internalServerError =<< appTemplate "Internal Server Error" ()
                                         <div id="main">
                                          <p><% userPassErrorString e %></p>
                                         </div>
               Nothing ->
                   ok =<< appTemplate "Password Updated" ()
                    <div>
                     <p>Your password has updated.</p>
                    </div>

changePasswordForm :: (Functor v, MonadIO v, XMLGenerator m, EmbedAsAttr m (Attr String AuthURL)) => AcidState AuthState -> UserPass -> Form v [Input] String [XMLGenT m (HSX.XML m)] String
changePasswordForm authStateH userPass =
    fieldset $ ol $ oldPassword *> newPassword <* changeBtn
    where
      -- form elements
      oldPassword = 
          errors ++>
          (li $ label "old password: " ++> inputPassword `transform` checkAuth)

      checkAuth =
          transformEitherM $ \password ->
              do r <- query' authStateH (CheckUserPass (unUserName $ upName userPass) (Text.pack password))
                 case r of 
                   (Left e)  -> return (Left (userPassErrorString e))
                   (Right _) -> return (Right password)


      password1 = li $ label "new password: "         ++> inputPassword
      password2 = li $ label "new confirm password: " ++> inputPassword

--       newPassword :: StoryForm String
      newPassword = 
          errors ++> (minLengthString 6 $ 
                      ((((,) <$> password1 <*> password2)) `transform` samePassword))

      samePassword = 
          transformEither $ \(p1, p2) ->
              if p1 /= p2
               then (Left "Passwords do not match.")
               else (Right p1)

      changeBtn = li $ submit "change"
