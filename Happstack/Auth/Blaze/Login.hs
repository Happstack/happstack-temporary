{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
module Happstack.Auth.Blaze.Login where

import Control.Applicative        (Alternative, (<*>), (<$>), (<*), (*>), optional)
import Control.Monad              (replicateM, mplus)
import Control.Monad.Trans        (MonadIO(liftIO))
import Data.Acid                  (AcidState, query', update')
import Data.Maybe                 (mapMaybe)
import Data.Monoid                (mempty)
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
import Happstack.Server
import Text.Digestive
import Text.Digestive.Blaze.Html5
import Text.Digestive.Forms.Happstack
import Web.Authenticate.OpenId    (Identifier, authenticate, getForwardUrl)
import Web.Authenticate.OpenId.Providers (google, yahoo, livejournal, myspace)
import Web.Authenticate.Facebook  (Facebook)
import Web.Routes                 (RouteT, ShowURL, showURL, showURLParams, nestURL, URL)
import Text.Blaze.Html5            as H hiding (fieldset, ol, li, label, head)
import qualified Text.Blaze.Html5  as H
import Text.Blaze.Html5.Attributes as A hiding (label)

-- * AuthURL stuff

logoutPage :: (ShowURL m, URL m ~ AuthURL, Alternative m, Happstack m) => AcidState AuthState -> m Html
logoutPage authStateH =
    do deleteAuthCookie authStateH 
       url <- H.toValue <$> showURL A_Login
       return $ H.div ! A.id "happstack-authenticate" $
                   p $ do "You are now logged out. Click "
                          a ! href url $ "here"
                          " to log in again."

loginPage :: (ShowURL m, URL m ~ AuthURL, Happstack m) => Maybe Facebook -> m Html
loginPage mFacebook =
    do googleURL      <- H.toValue <$> showURL (A_OpenIdProvider LoginMode Google)
       yahooURL       <- H.toValue <$> showURL (A_OpenIdProvider LoginMode Yahoo)
       liveJournalURL <- H.toValue <$> showURL (A_OpenIdProvider LoginMode LiveJournal)
       myspaceURL     <- H.toValue <$> showURL (A_OpenIdProvider LoginMode Myspace)
       genericURL     <- H.toValue <$> showURL (A_OpenIdProvider LoginMode Generic)
       localURL       <- H.toValue <$> showURL A_Local
       facebookURL    <- H.toValue <$> showURL (A_Facebook LoginMode)
       return $ H.div ! A.id "happstack-authenticate" $
                  H.ol $ do 
                    H.li $ (a ! href googleURL      $ "Login") >> " with your Google account"
                    H.li $ (a ! href yahooURL       $ "Login") >> " with your Yahoo account"
                    H.li $ (a ! href liveJournalURL $ "Login") >> " with your Live Journal account"
                    H.li $ (a ! href myspaceURL     $ "Login") >> " with your Myspace account"
                    H.li $ (a ! href myspaceURL     $ "Login") >> " with your OpenId account"
                    H.li $ (a ! href myspaceURL     $ "Login") >> " with a username and password"
                    case mFacebook of
                      (Just _) -> H.li $ (a ! href facebookURL $ "Login") >> " with your Facebook account"
                      Nothing -> return ()

addAuthPage :: (ShowURL m, URL m ~ AuthURL, Happstack m) => Maybe Facebook -> m Html
addAuthPage mFacebook =
    do googleURL      <- H.toValue <$> showURL (A_OpenIdProvider AddIdentifierMode Google)
       yahooURL       <- H.toValue <$> showURL (A_OpenIdProvider AddIdentifierMode Yahoo)
       liveJournalURL <- H.toValue <$> showURL (A_OpenIdProvider AddIdentifierMode LiveJournal)
       myspaceURL     <- H.toValue <$> showURL (A_OpenIdProvider AddIdentifierMode Myspace)
       genericURL     <- H.toValue <$> showURL (A_OpenIdProvider AddIdentifierMode Generic)
       facebookURL    <- H.toValue <$> showURL (A_Facebook AddIdentifierMode)
       return $ H.div ! A.id "happstack-authenticate" $
                  H.ol $ do 
                    H.li $ (a ! href googleURL      $ "Add") >> " your Google account"
                    H.li $ (a ! href yahooURL       $ "Add") >> " your Yahoo account"
                    H.li $ (a ! href liveJournalURL $ "Add") >> " your Live Journal account"
                    H.li $ (a ! href myspaceURL     $ "Add") >> " your Myspace account"
                    H.li $ (a ! href myspaceURL     $ "Add") >> " your OpenId account"
                    case mFacebook of
                      (Just _) -> H.li $ (a ! href facebookURL $ "Add") >> " your Facebook account"
                      Nothing -> return ()

authPicker :: (ShowURL m, URL m ~ ProfileURL, Happstack m) => Set AuthId -> m Html
authPicker authIds =
    do auths <- mapM auth (Set.toList authIds)
       return $ H.div ! A.id "happstack-authenticate" $
                   ul $ sequence_ auths
    where
      auth authId =
          do url <- H.toValue <$> showURL (P_SetAuthId authId)
             return $ H.li $ a ! href url $ (H.toHtml $ show authId) -- FIXME: give a more informative view. 
      
personalityPicker :: (ShowURL m, URL m ~ ProfileURL, Happstack m) => 
                     Set Profile 
                  -> m Html
personalityPicker profiles =
    do personalities <- mapM personality (Set.toList profiles)
       return $ H.div ! A.id "happstack-authenticate" $
                   ul $ sequence_ personalities
    where
      personality profile =
          do url <- H.toValue <$> showURL (P_SetPersonality (userId profile))
             return $ H.li $ a ! href url $ (H.toHtml $ nickName profile)

providerPage :: (URL m ~ AuthURL, Happstack m, ShowURL m) =>
                (String -> Html -> Html -> m Response)
             -> OpenIdProvider
             -> AuthURL
             -> AuthMode
             -> m Response
providerPage appTemplate provider =
    case provider of
      Google      -> googlePage
      Yahoo       -> yahooPage
      LiveJournal -> liveJournalPage   appTemplate
      Myspace     -> myspacePage       appTemplate
      Generic     -> genericOpenIdPage appTemplate

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

myspacePage :: (Happstack m, ShowURL m, URL m ~ AuthURL) =>
               (String -> Html -> Html -> m Response) 
            -> AuthURL
            -> AuthMode
            -> m Response
myspacePage appTemplate here authMode =
    do actionURL <- showURL here
       e <- eitherHappstackForm liveJournalForm "msp"
       case e of
         (Left formHtml) ->
             do r <- appTemplate "Login via Myspace" mempty $
                       H.div ! A.id "happstack-authenticate" $
                        do h1 "Login using your myspace account"
                           p "Enter your Myspace account name to connect."
                           formToHtml actionURL formHtml
                ok r
         (Right username) ->
             do u <- showURLParams (A_OpenId (O_Connect authMode)) [("url", myspace username)]
                seeOther u (toResponse ())
      where 
        usernameForm :: (Functor v, Monad v) => Form v [Input] Html BlazeFormHtml String
        usernameForm = 
            label "http://www.myspace.com/" ++> inputText Nothing <* (mapHtml (\html -> html ! A.class_  "submit") $ submit "Login")

liveJournalPage :: (Happstack m, ShowURL m, URL m ~ AuthURL) =>
                   (String -> Html -> Html -> m Response)
                -> AuthURL
                -> AuthMode
                -> m Response
liveJournalPage appTemplate here authMode =
    do actionURL <- showURL here
       e <- eitherHappstackForm liveJournalForm "ljp"
       case e of 
         (Left formHtml) ->
             do r <- appTemplate "Login via LiveJournal" mempty $
                     H.div ! A.id "happstack-authenticate" $
                      do h1 $ "Login using your Live Journal account"
                         p $ "Enter your livejournal account name to connect. You may be prompted to log into your livejournal account and to confirm the login."
                         formToHtml actionURL formHtml
                ok r
         (Right username) ->
             do u <- showURLParams (A_OpenId (O_Connect authMode)) [("url", livejournal username)]
                seeOther u (toResponse ())

liveJournalForm :: (Functor v, Monad v) => Form v [Input] Html BlazeFormHtml String
liveJournalForm = 
    label "http://" ++> inputText Nothing <++ label ".livejournal.com/" <* (mapHtml (\html -> html ! A.class_  "submit") $ submit "Connect")


genericOpenIdPage :: (Happstack m, ShowURL m, URL m ~ AuthURL) =>
                     (String -> Html -> Html -> m Response)
                  -> AuthURL
                  -> AuthMode
                  -> m Response
genericOpenIdPage appTemplate here authMode =
    do actionURL <- showURL here
       e <- eitherHappstackForm openIdURLForm "oiu"
       case e of
         (Left formHtml) ->
             do r <- appTemplate "Login via Generic OpenId" mempty $
                       H.div ! A.id "happstack-authenticate" $
                        do h1 "Login using your OpenId account"
                           formToHtml actionURL formHtml
                ok r
         (Right url) ->
             do u <- showURLParams (A_OpenId (O_Connect authMode)) [("url", url)]
                seeOther u (toResponse ())
      where 
        openIdURLForm :: (Functor v, Monad v) => Form v [Input] Html BlazeFormHtml String
        openIdURLForm = 
            label "Your OpenId url: " ++> inputText Nothing <* submit "Connect"


handleAuth :: (Happstack m) =>
              AcidState AuthState
           -> (String -> Html -> Html -> RouteT AuthURL m Response)
           -> Maybe Facebook
           -> Maybe String
           -> String
           -> AuthURL
           -> RouteT AuthURL m Response
handleAuth authStateH appTemplate mFacebook realm onAuthURL url =
    case url of
      A_Login           -> appTemplate "Login"    mempty =<< loginPage mFacebook
      A_AddAuth         -> appTemplate "Add Auth" mempty =<< addAuthPage mFacebook
      A_Logout          -> appTemplate "Logout"   mempty =<< logoutPage authStateH

      A_Local           -> localLoginPage authStateH appTemplate url onAuthURL
      A_CreateAccount   -> createAccountPage authStateH appTemplate onAuthURL url
      A_ChangePassword  -> changePasswordPage authStateH appTemplate url
      (A_OpenId oidURL) -> nestURL A_OpenId $ handleOpenId authStateH realm onAuthURL oidURL

      (A_OpenIdProvider authMode provider) 
                        -> providerPage appTemplate provider url authMode


      (A_Facebook authMode) 
                        -> case mFacebook of
                             Nothing -> do resp <- appTemplate "Facebook authentication not configured." mempty $
                                                     H.div ! A.id "happstack-authenticate" $
                                                      p "Facebook authentication not configured."
                                           internalServerError resp
                             (Just facebook) -> facebookPage facebook authMode 

      (A_FacebookRedirect authMode)
                        -> case mFacebook of
                             Nothing -> do resp <- appTemplate "Facebook authentication not configured." mempty $
                                                     H.div ! A.id "happstack-authenticate" $
                                                      p "Facebook authentication not configured."
                                           internalServerError resp
                             (Just facebook) -> facebookRedirectPage authStateH facebook onAuthURL authMode

handleProfile :: (Happstack m, Alternative m, ShowURL m, URL m ~ ProfileURL) =>
                 AcidState AuthState
              -> AcidState ProfileState
              -> (String -> Html -> Html -> m Response)
              -> String
              -> ProfileURL 
              -> m Response
handleProfile authStateH profileStateH appTemplate postPickedURL url =
    case url of
      P_PickProfile        -> 
          do r <- pickProfile authStateH profileStateH
             case r of
               (Picked {})                -> 
                   seeOther postPickedURL (toResponse postPickedURL)

               (PickPersonality profiles) -> 
                   appTemplate "Pick Personality" mempty =<< (personalityPicker profiles)

               (PickAuthId      authIds)  ->
                   appTemplate "Pick Auth" mempty =<< (authPicker authIds) 

      (P_SetAuthId authId) -> 
          do b <- setAuthIdPage authStateH authId
             if b
              then seeOther ("/" :: String) (toResponse ()) -- FIXME: don't hardcode destination
              else do resp <-  appTemplate "Unauthorized" mempty $
                                 H.div ! A.id "happstack-authenticate" $
                                   p $ do " Attempted to set AuthId to "
                                          toHtml $show $ unAuthId authId
                                          ", but failed because the Identifier is not associated with that AuthId."
                      unauthorized resp

localLoginPage authStateH appTemplate here onAuthURL =
    do actionURL <- showURL here
       createURL <- showURL A_CreateAccount
       e <- eitherHappstackForm (loginForm createURL) "lf"
       case e of 
         (Left formHtml) ->
             do r <- appTemplate "Login" mempty $
                     H.div ! A.id "happstack-authenticate" $
                      do h1 "Login"
                         formToHtml actionURL formHtml
                ok r
         (Right userPassId) ->
            do authId <- do authIds <- query' authStateH (UserPassIdAuthIds userPassId) 
                            case Set.size authIds of
                              1 -> return (Just $ head $ Set.toList $ authIds)
                              n -> return Nothing
               addAuthCookie authStateH authId (AuthUserPassId userPassId)
               seeOther onAuthURL (toResponse ())

      where 
        loginForm createURL =
            (errors ++> (fieldset $ ol (((,) <$> (li $ label "username: " ++> (Text.pack <$> inputText Nothing)) <*> (li $ label "password: " ++> inputPassword) <* login) `transform` checkAuth))) <* (create createURL)

        create createURL = viewHtml $ p $ do "or "
                                             H.a ! href (toValue createURL) $ "create a new account"
        login = li $ mapHtml (\html -> html ! A.class_  "submit") (submit "Login")

        checkAuth = 
            transformEitherM $ \(username, password) ->
                do r <- query' authStateH (CheckUserPass username (Text.pack password))
                   case r of 
                     (Left e) -> return (Left $ toHtml $ userPassErrorString e)
                     (Right userPassId) -> return (Right userPassId)

createAccountPage :: (Happstack m, ShowURL m, URL m ~ AuthURL) => AcidState AuthState -> (String -> Html -> Html -> m Response) -> String -> AuthURL -> m Response
createAccountPage authStateH appTemplate onAuthURL here =
    do actionURL <- showURL here
       e <- eitherHappstackForm (newAccountForm authStateH) "naf"
       case e of
         (Left formHtml) ->
             do r <- appTemplate "Create New Account" mempty $
                     H.div ! A.id "happstack-authenticate" $
                      do h1 "Create an account"
                         formToHtml actionURL formHtml
                ok r
         (Right (authId, userPassId)) ->
             do addAuthCookie authStateH (Just authId) (AuthUserPassId userPassId)
                seeOther onAuthURL (toResponse ())

newAccountForm :: (Functor v, MonadIO v) => AcidState AuthState -> Form v [Input] Html BlazeFormHtml (AuthId, UserPassId)
newAccountForm authStateH =
    fieldset (errors ++> (ol $ ((,) <$> username <*> password <* submitButton)
                `transform`
                createAccount))
    where
      submitButton = li $ (mapHtml (\html -> html ! A.class_  "submit") $ submit "Create Account")
      username  = li $ ((label "username: "         ++> (Text.pack <$> inputText Nothing)  <++ errors) `validate` notEmpty)
      password1 = li $ label "password: "         ++> inputPassword <++ errors
      password2 = li $ label "confirm password: " ++> inputPassword <++ errors

      password = 
          (minLengthString 6 $ 
           (((,) <$> password1 <*> password2) `transform` samePassword))

      samePassword = 
          transformEither $ \(p1, p2) ->
              if p1 /= p2
               then (Left $ p "Passwords do not match.")
               else (Right p1)

      createAccount = 
          transformEitherM $ \(username, password) ->
              do passHash <- liftIO $ mkHashedPass (Text.pack password)
                 r <- update' authStateH $ CreateUserPass (UserName username) passHash
                 -- fixme: race condition
                 case r of
                   (Left e) -> return (Left $ p $ toHtml (userPassErrorString e))
                   (Right userPass) -> 
                       do authId <- update' authStateH (NewAuthMethod (AuthUserPassId (upId userPass)))
                          return (Right (authId, upId userPass))

fieldset, ol, li :: (Functor v, Monad v) => Form v [Input] e BlazeFormHtml a -> Form v [Input] e BlazeFormHtml a
fieldset = mapHtml (\html -> H.fieldset $ html)
ol       = mapHtml (\html -> H.ol $ html)
li       = mapHtml (\html -> H.li $ html)

mapHtml :: (Functor v, Monad v) => (Html -> Html) -> Form v i e BlazeFormHtml a -> Form v i e BlazeFormHtml a
mapHtml fn = mapView fn'
    where
      fn' :: (BlazeFormHtml -> BlazeFormHtml)
      fn' (FormHtml encType formHtml) =
          let formHtml' = \formHtmlConfig -> fn (formHtml formHtmlConfig)
          in (FormHtml encType formHtml')
    
notEmpty :: (Monad m) => Validator m Html Text
notEmpty = (check $ H.span "field can not be empty") (not . Text.null)

minLengthString :: (Monad v) => Int -> Form v [Input] Html BlazeFormHtml String -> Form v [Input] Html BlazeFormHtml String
minLengthString 0 f = f
minLengthString 1 f = errors ++> (f `validate` (check ("This field can not be empty.") (not . null)))
minLengthString n f = errors ++> (f `validate` (check (toHtml $ "This field must be at least " ++ show n ++ " characters.") (\t -> length t >= n)))

formToHtml :: String -> FormHtml Html -> Html
formToHtml actionURL formHtml =
    let (formHtml', enctype) = renderFormHtml formHtml
    in H.form ! A.enctype (toValue $ show enctype) 
              ! A.method "POST" 
              ! A.action (toValue actionURL) $
             formHtml'

changePasswordPage :: (Happstack m, ShowURL m, URL m ~ AuthURL) =>
                      AcidState AuthState -> (String -> Html -> Html -> m Response) -> AuthURL -> m Response
changePasswordPage authStateH appTemplate here =
    do actionURL  <- showURL here 
       mAuthToken <- getAuthToken authStateH
       case mAuthToken of
         Nothing -> seeOtherURL A_Login
         (Just authToken) -> 
             case tokenAuthMethod authToken of
               (AuthUserPassId userPassId) ->
                   do mUserPass <- query' authStateH (AskUserPass userPassId)
                      case mUserPass of
                        Nothing ->
                            do resp <- appTemplate "Invalid UserPassId" mempty $ do H.div ! A.id "happstack-authenticate" $
                                                                                         p $ do "Invalid UserPassId"
                                                                                                toHtml $ show $ unUserPassId userPassId
                               internalServerError resp
                        (Just userPass) ->
                            do e <- eitherHappstackForm (changePasswordForm authStateH userPass) "cpf"
                               case e of
                                 (Left formHtml) ->
                                    do r <- appTemplate "Change Passowrd" mempty $ 
                                                 H.div ! A.id "happstack-authenticate" $
                                                     do h1 $ do "Change password for " 
                                                                toHtml $ unUserName $ upName userPass
                                                                formToHtml actionURL formHtml
                                       ok r
                                 (Right passwd) ->
                                    do hashedPass <- liftIO $ mkHashedPass (Text.pack passwd)
                                       r <- update' authStateH (SetPassword userPassId hashedPass)
                                       case r of
                                         (Just e) -> 
                                             do resp <- appTemplate "Change Password Failed" mempty $
                                                          H.div ! A.id "happstack-authenticate" $
                                                           p $ toHtml (userPassErrorString e)
                                                internalServerError resp
                                         Nothing ->
                                             do resp <- appTemplate "Password Changed!" mempty $
                                                          H.div ! A.id "happstack-authenticate" $
                                                           p $ "Your password has been updated."
                                                ok resp

changePasswordForm  :: (Functor v, MonadIO v) => AcidState AuthState -> UserPass -> Form v [Input] Html BlazeFormHtml String
changePasswordForm authStateH userPass =
    fieldset $ ol $ oldPassword *> newPassword <* changeBtn
    where
      -- form elements
      oldPassword = 
          errors ++>
          (li $  label "old password: " ++> inputPassword `transform` checkAuth)
      checkAuth =
          transformEitherM $ \password ->
              do r <- query' authStateH (CheckUserPass (unUserName $ upName userPass) (Text.pack password))
                 case r of 
                   (Left e)  -> return (Left $ toHtml (userPassErrorString e))
                   (Right _) -> return (Right password)

      password1, password2 :: (Functor v, Monad v) => Form v [Input] Html BlazeFormHtml String
      password1 = li $ label "new password: "         ++> inputPassword
      password2 = li $ label "new confirm password: " ++> inputPassword

      newPassword :: (Functor v, Monad v) => Form v [Input] Html BlazeFormHtml String
      newPassword = 
          errors ++> (minLengthString 6 $ 
                      ((((,) <$> password1 <*> password2)) `transform` samePassword))

      samePassword :: (Monad m) => Transformer m Html (String, String) String
      samePassword = 
          transformEither $ \(p1, p2) ->
              if p1 /= p2
               then (Left $ "Passwords do not match.")
               else (Right p1)

      changeBtn :: (Functor v, Monad v) => Form v [Input] Html BlazeFormHtml ()
      changeBtn = li $ mapHtml (\html -> html ! A.class_  "submit") $ submit "change"
