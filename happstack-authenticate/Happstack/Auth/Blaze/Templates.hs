{-# LANGUAGE FlexibleInstances, RankNTypes, TypeFamilies, OverloadedStrings #-}
-- | This modules provides templates and routing functions which can
-- be used to integrate authentication into your site.
--
-- In most cases, you only need to call the 'handleAuth' and
-- 'hanldeProfile' functions. The other functions are exported in case
-- you wish to create your own alternatives to 'handleAuth' \/
-- 'handleProfile'
--
module Happstack.Auth.Blaze.Templates
    ( -- * handlers
      handleAuth
    , handleProfile
    , handleAuthProfile
    , authProfileHandler
      -- * page functions
    , addAuthPage
    , authPicker
    , createAccountPage
    , googlePage
    , genericOpenIdPage
    , yahooPage
    , liveJournalPage
    , liveJournalForm
    , myspacePage
    , localLoginPage
    , newAccountForm
    , personalityPicker
    , providerPage
    , loginPage
    , logoutPage
    , changePasswordPage
    , changePasswordForm
    ) where

import Control.Applicative        (Alternative, (<*>), (<$>), (<*), (*>), optional)
import Control.Monad              (replicateM, mplus, mzero)
import Control.Monad.Trans        (MonadIO(liftIO))
import Data.Acid                  (AcidState)
import Data.Acid.Advanced         (query', update')
import Data.Maybe                 (mapMaybe)
import Data.Monoid                (mempty)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import Data.Text                  (Text)
import qualified Data.Text        as Text
import Data.Time.Clock            (getCurrentTime)
import Facebook                   (Credentials)
import Happstack.Auth.Core.Auth
import Happstack.Auth.Core.AuthParts
import Happstack.Auth.Core.AuthURL
import Happstack.Auth.Core.ProfileURL
import Happstack.Auth.Core.Profile
import Happstack.Auth.Core.ProfileParts
import Happstack.Auth.Core.AuthProfileURL (AuthProfileURL(..))
import Happstack.Server            (Happstack, Input, Response, internalServerError, ok, seeOther, toResponse, unauthorized)
import Text.Blaze.Html5            as H hiding (fieldset, ol, li, label, head)
import qualified Text.Blaze.Html5  as H
import Text.Blaze.Html5.Attributes as A hiding (label)
import Text.Reform
import Text.Reform.Blaze.Text     as R
import Text.Reform.Happstack      as R
import Web.Authenticate.OpenId    (Identifier, authenticate, getForwardUrl)
import Web.Authenticate.OpenId.Providers (google, yahoo, livejournal, myspace)
import Web.Routes                 (RouteT(..), Site(..), PathInfo(..), MonadRoute(askRouteFn), parseSegments, showURL, showURLParams, nestURL, liftRouteT, URL)
import Web.Routes.Happstack       (implSite_, seeOtherURL)

smap :: (String -> String) -> Text -> Text
smap f = Text.pack . f . Text.unpack

data AuthTemplateError
    = ATECommon (CommonFormError [Input])
    | UPE UserPassError
    | MinLength Int
    | PasswordMismatch

instance FormError AuthTemplateError where
    type ErrorInputType AuthTemplateError = [Input]
    commonFormError = ATECommon

instance ToMarkup (CommonFormError [Input]) where
    toMarkup e = toMarkup $ show e

instance ToMarkup AuthTemplateError where
    toMarkup (ATECommon e)    = toHtml $ e
    toMarkup (UPE e)          = toHtml $ userPassErrorString e
    toMarkup (MinLength n)    = toHtml $ "mimimum length: " ++ show n
    toMarkup PasswordMismatch = "Passwords do not match."

type AuthForm m a = Form m [Input] AuthTemplateError Html () a

logoutPage :: (MonadRoute m, URL m ~ AuthURL, Alternative m, Happstack m) => AcidState AuthState -> m Html
logoutPage authStateH =
    do deleteAuthCookie authStateH
       url <- H.toValue <$> showURL A_Login
       return $ H.div ! A.id "happstack-authenticate" $
                   p $ do "You are now logged out. Click "
                          a ! href url $ "here"
                          " to log in again."

loginPage :: (MonadRoute m, URL m ~ AuthURL, Happstack m) => Maybe Credentials -> m Html
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
                    H.li $ (a ! href genericURL     $ "Login") >> " with your OpenId account"
                    H.li $ (a ! href localURL       $ "Login") >> " with a username and password"
                    case mFacebook of
                      (Just _) -> H.li $ (a ! href facebookURL $ "Login") >> " with your Facebook account"
                      Nothing -> return ()

addAuthPage :: (MonadRoute m, URL m ~ AuthURL, Happstack m) => Maybe Credentials -> m Html
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
                    H.li $ (a ! href genericURL     $ "Add") >> " your OpenId account"
                    case mFacebook of
                      (Just _) -> H.li $ (a ! href facebookURL $ "Add") >> " your Facebook account"
                      Nothing -> return ()

authPicker :: (MonadRoute m, URL m ~ ProfileURL, Happstack m) => Set AuthId -> m Html
authPicker authIds =
    do auths <- mapM auth (Set.toList authIds)
       return $ H.div ! A.id "happstack-authenticate" $
                   H.ul $ sequence_ auths
    where
      auth authId =
          do url <- H.toValue <$> showURL (P_SetAuthId authId)
             return $ H.li $ a ! href url $ (H.toHtml $ show authId) -- FIXME: give a more informative view.

personalityPicker :: (MonadRoute m, URL m ~ ProfileURL, Happstack m) =>
                     Set Profile
                  -> m Html
personalityPicker profiles =
    do personalities <- mapM personality (Set.toList profiles)
       return $ H.div ! A.id "happstack-authenticate" $
                   H.ul $ sequence_ personalities
    where
      personality profile =
          do url <- H.toValue <$> showURL (P_SetPersonality (userId profile))
             return $ H.li $ a ! href url $ (H.toHtml $ nickName profile)

providerPage :: (URL m ~ AuthURL, Happstack m, MonadRoute m) =>
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

googlePage :: (Happstack m, MonadRoute m, URL m ~ AuthURL) =>
              AuthURL
           -> AuthMode
           -> m Response
googlePage _here authMode =
    do u <- showURLParams (A_OpenId (O_Connect authMode)) [("url", Just $ Text.pack google)]
       seeOther (Text.unpack u) (toResponse ())

yahooPage :: (Happstack m, MonadRoute m, URL m ~ AuthURL) =>
             AuthURL
          -> AuthMode
          -> m Response
yahooPage _here authMode =
    do u <- showURLParams (A_OpenId (O_Connect authMode)) [(Text.pack "url", Just $ Text.pack yahoo)]
       seeOther (Text.unpack u) (toResponse ())

myspacePage :: (Happstack m, MonadRoute m, URL m ~ AuthURL) =>
               (String -> Html -> Html -> m Response)
            -> AuthURL
            -> AuthMode
            -> m Response
myspacePage appTemplate here authMode =
    do actionURL <- showURL here
       e <- happstackEitherForm (R.form actionURL) "msp" usernameForm
       case e of
         (Left formHtml) ->
             do r <- appTemplate "Login via Myspace" mempty $
                       H.div ! A.id "happstack-authenticate" $
                        do h1 "Login using your myspace account"
                           p "Enter your Myspace account name to connect."
                           formHtml
                ok r
         (Right username) ->
             do u <- showURLParams (A_OpenId (O_Connect authMode)) [("url", Just $ smap myspace username)]
                seeOther (Text.unpack u) (toResponse ())

      where
        usernameForm :: (Functor m, MonadIO m) => AuthForm m Text
        usernameForm =
              divInline (label' "http://www.myspace.com/" ++> inputText mempty)
           <* (divFormActions $ inputSubmit' "Login")


liveJournalPage :: (Happstack m, MonadRoute m, URL m ~ AuthURL) =>
                   (String -> Html -> Html -> m Response)
                -> AuthURL
                -> AuthMode
                -> m Response
liveJournalPage appTemplate here authMode =
    do actionURL <- showURL here
       e <- happstackEitherForm (R.form actionURL) "ljp" liveJournalForm
       case e of
         (Left formHtml) ->
             do r <- appTemplate "Login via LiveJournal" mempty $
                     H.div ! A.id "happstack-authenticate" $
                      do h1 $ "Login using your Live Journal account"
                         p $ "Enter your livejournal account name to connect. You may be prompted to log into your livejournal account and to confirm the login."
                         formHtml
                ok r
         (Right username) ->
             do u <- showURLParams (A_OpenId (O_Connect authMode)) [("url", Just $ smap livejournal username)]
                seeOther (Text.unpack u) (toResponse ())

liveJournalForm :: (Functor m, MonadIO m) => AuthForm m Text
liveJournalForm =
      divInline (label' "http://" ++> inputText mempty <++ label' ".livejournal.com/")
   <* divFormActions (inputSubmit' "Connect")

genericOpenIdPage :: (Happstack m, MonadRoute m, URL m ~ AuthURL) =>
                     (String -> Html -> Html -> m Response)
                  -> AuthURL
                  -> AuthMode
                  -> m Response
genericOpenIdPage appTemplate here authMode =
    do actionURL <- showURL here
       e <- happstackEitherForm (R.form actionURL) "oiu" openIdURLForm
       case e of
         (Left formHtml) ->
             do r <- appTemplate "Login via Generic OpenId" mempty $
                       H.div ! A.id "happstack-authenticate" $
                        do h1 "Login using your OpenId account"
                           formHtml
                ok r
         (Right url) ->
             do u <- showURLParams (A_OpenId (O_Connect authMode)) [("url", Just url)]
                seeOther (Text.unpack u) (toResponse ())
      where
        openIdURLForm :: (Functor m, MonadIO m) => AuthForm m Text
        openIdURLForm =
            divInline (label' ("Your OpenId url: " :: String) ++> inputText mempty) <*
            divFormActions (inputSubmit "Connect")

-- | Function which takes care of all 'AuthURL' routes.
--
-- The caller provides a page template function which will be used to
-- render pages. The provided page template function takes three
-- arguments:
--
--  >    String -- ^ string to use in the <title> tag
--  > -> Html   -- ^ extra headers to add to the <head> tag
--  > -> Html   -- ^ contents to stick in the <body> tag
handleAuth :: (Happstack m, MonadRoute m, URL m ~ AuthURL) =>
              AcidState AuthState -- ^ database handle for 'AuthState'
           -> (String -> Html -> Html -> m Response) -- ^ page template function
           -> Maybe Credentials      -- ^ config information for facebook connect
           -> Maybe Text          -- ^ authentication realm
           -> Text                -- ^ URL to redirect to after succesful authentication
           -> AuthURL             -- ^ url to route
           -> m Response
handleAuth authStateH appTemplate mFacebook realm onAuthURL url =
    case url of
      A_Login           -> appTemplate "Login"    mempty =<< loginPage mFacebook
      A_AddAuth         -> appTemplate "Add Auth" mempty =<< addAuthPage mFacebook
      A_Logout          -> appTemplate "Logout"   mempty =<< logoutPage authStateH

      A_Local           -> localLoginPage authStateH appTemplate url onAuthURL
      A_CreateAccount   -> createAccountPage authStateH appTemplate onAuthURL url
      A_ChangePassword  -> changePasswordPage authStateH appTemplate url

      (A_OpenId oidURL) -> do showFn <- askRouteFn
                              unRouteT (nestURL A_OpenId $ handleOpenId authStateH realm onAuthURL oidURL) showFn

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

-- | Function which takes care of all 'ProfileURL' routes.
--
-- The caller provides a page template function which will be used to
-- render pages. The provided page template function takes three
-- arguments:
--
--  >    String -- ^ string to use in the <title> tag
--  > -> Html   -- ^ extra headers to add to the <head> tag
--  > -> Html   -- ^ contents to stick in the <body> tag
handleProfile :: (Happstack m, Alternative m, MonadRoute m, URL m ~ ProfileURL) =>
                 AcidState AuthState    -- ^ database handle for 'AuthState'
              -> AcidState ProfileState -- ^ database handle for 'ProfileState'
              -> (String -> Html -> Html -> m Response) -- ^ page template function
              -> Text -- ^ URL to redirect to after successfully picking an identity
              -> ProfileURL -- ^ URL to route
              -> m Response
handleProfile authStateH profileStateH appTemplate postPickedURL url =
    case url of
      P_PickProfile        ->
          do r <- pickProfile authStateH profileStateH
             case r of
               (Picked {})                ->
                   seeOther (Text.unpack postPickedURL) (toResponse postPickedURL)

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
                                          toHtml $ show $ unAuthId authId
                                          ", but failed because the Identifier is not associated with that AuthId."
                      unauthorized resp


-- handleAuthProfile :: (Happstack m, Alternative m, MonadRoute m, URL m ~ AuthProfileURL) =>

authProfileSite :: (Happstack m) =>
                   AcidState AuthState
                -> AcidState ProfileState
                -> (String  -> Html -> Html -> m Response)
                -> Maybe Credentials
                -> Maybe Text
                -> Text
                -> Site AuthProfileURL (m Response)
authProfileSite acidAuth acidProfile appTemplate mFacebook realm postPickedURL
    = Site { handleSite = \f u -> unRouteT (handleAuthProfileRouteT acidAuth acidProfile appTemplate mFacebook realm postPickedURL u) f
           , formatPathSegments = \u -> (toPathSegments u, [])
           , parsePathSegments  = parseSegments fromPathSegments
           }

-- | this is a simple entry point into @happstack-authenticate@ that
-- provides reasonable default behavior. A majority of the time you
-- will just call this function.
authProfileHandler :: (Happstack m) =>
                      Text -- ^ baseURI for this server part
                   -> Text -- ^ unique path prefix
                   -> AcidState AuthState                     -- ^ handle for 'AcidState AuthState'
                   -> AcidState ProfileState                  -- ^ handle for 'AcidState ProfileState'
                   -> (String  -> Html -> Html -> m Response) -- ^ template function used to render pages
                   -> Maybe Credentials                       -- ^ optional Facebook 'Credentials'
                   -> Maybe Text                              -- ^ optional realm to use for @OpenId@ authentication
                   -> Text                                    -- ^ url to redirect to if authentication and profile selection is successful
                   -> m Response
authProfileHandler baseURI pathPrefix acidAuth acidProfile appTemplate mFacebook realm postPickedURL =
    do r <- implSite_ baseURI pathPrefix (authProfileSite acidAuth acidProfile appTemplate mFacebook realm postPickedURL)
       case r of
         (Left e) -> mzero
         (Right r) -> return r

handleAuthProfile :: forall m. (Happstack m, MonadRoute m, URL m ~ AuthProfileURL) =>
                     AcidState AuthState
                  -> AcidState ProfileState
                  -> (String -> Html -> Html -> m Response)
                  -> Maybe Credentials
                  -> Maybe Text
                  -> Text
                  -> AuthProfileURL
                  -> m Response
handleAuthProfile authStateH profileStateH appTemplate mFacebook mRealm postPickedURL url =
    do routeFn <- askRouteFn
       unRouteT (handleAuthProfileRouteT authStateH profileStateH appTemplate mFacebook mRealm postPickedURL url) routeFn

handleAuthProfileRouteT :: forall m. (Happstack m) =>
                     AcidState AuthState
                  -> AcidState ProfileState
                  -> (String -> Html -> Html -> m Response)
                  -> Maybe Credentials
                  -> Maybe Text
                  -> Text
                  -> AuthProfileURL
                  -> RouteT AuthProfileURL m Response
handleAuthProfileRouteT authStateH profileStateH appTemplate mFacebook mRealm postPickedURL url =
    case url of
      (AuthURL authURL) ->
          do onAuthURL <- showURL (ProfileURL P_PickProfile)
             let template t h b = liftRouteT (appTemplate t h b)
             nestURL AuthURL $ handleAuth authStateH template mFacebook mRealm onAuthURL authURL
      (ProfileURL profileURL) ->
          do let template t h b = liftRouteT (appTemplate t h b)
             nestURL ProfileURL $ handleProfile authStateH profileStateH template postPickedURL profileURL

localLoginPage authStateH appTemplate here onAuthURL =
    do actionURL <- showURL here
       createURL <- showURL A_CreateAccount
       e <- happstackEitherForm (R.form actionURL) "lf" (loginForm createURL)
       case e of
         (Left errorForm) ->
             do r <- appTemplate "Login" mempty $
                     H.div ! A.id "happstack-authenticate" $
                      do h1 "Login"
                         errorForm
                ok r
         (Right userPassId) ->
            do authId <- do authIds <- query' authStateH (UserPassIdAuthIds userPassId)
                            case Set.size authIds of
                              1 -> return (Just $ head $ Set.toList $ authIds)
                              n -> return Nothing
               addAuthCookie authStateH authId (AuthUserPassId userPassId)
               seeOther (Text.unpack onAuthURL) (toResponse ())

      where
        loginForm createURL =
            divHorizontal $
             fieldset $
              (errorList ++>
                (((,) <$> (divControlGroup $ errorList ++> label' "username: " ++> divControls (inputText mempty))
                      <*> (divControlGroup $ errorList ++> label' "password: " ++> divControls (inputPassword))
                           <* divFormActions (inputSubmit' "Login")) `transformEitherM` checkAuth)
                      <* (create createURL))

        create createURL = view $ p $ do "or "
                                         H.a ! href (toValue createURL) $ "create a new account"

        checkAuth :: (MonadIO m) => (Text, Text) -> m (Either AuthTemplateError UserPassId)
        checkAuth (username, password) =
                do r <- query' authStateH (CheckUserPass username password)
                   case r of
                     (Left e) -> return (Left $ UPE e)
                     (Right userPassId) -> return (Right userPassId)

createAccountPage :: (Happstack m, MonadRoute m, URL m ~ AuthURL) => AcidState AuthState -> (String -> Html -> Html -> m Response) -> Text -> AuthURL -> m Response
createAccountPage authStateH appTemplate onAuthURL here =
    do actionURL <- showURL here
       e <- happstackEitherForm (R.form actionURL) "naf" (newAccountForm authStateH)
       case e of

         (Left formHtml) ->
             do r <- appTemplate "Create New Account" mempty $
                     H.div ! A.id "happstack-authenticate" $
                      do h1 "Create an account"
                         formHtml
                ok r

         (Right (authId, userPassId)) ->
             do addAuthCookie authStateH (Just authId) (AuthUserPassId userPassId)
                seeOther (Text.unpack onAuthURL) (toResponse ())

newAccountForm :: (Functor v, MonadIO v) => AcidState AuthState -> AuthForm v (AuthId, UserPassId)
newAccountForm authStateH =
    divHorizontal $
     (R.fieldset
      (errorList ++>
       (((,) <$> username <*> password <* submitButton)))
                     `transformEitherM`
                     createAccount)
    where
      submitButton = divFormActions $ inputSubmit' "Create Account"
      username  = divControlGroup $ errorList ++> ((label' "username: " ++> divControls (inputText mempty)) `transformEither` (minLength 1))
      password1 = divControlGroup $ label' "password: "          ++> divControls inputPassword
      password2 = divControlGroup $ label' "confirm password: "  ++> divControls inputPassword

      password =
          errorList ++> (((,) <$> password1 <*> password2) `transformEither` samePassword) `transformEither` minLength 6

      samePassword (p1, p2) =
              if p1 /= p2
               then (Left $ PasswordMismatch)
               else (Right p1)

      createAccount (username, password) =
              do passHash <- liftIO $ mkHashedPass password
                 r <- update' authStateH $ CreateUserPass (UserName username) passHash
                 -- fixme: race condition
                 case r of
                   (Left e) -> return (Left $ UPE e)
                   (Right userPass) ->
                       do authId <- update' authStateH (NewAuthMethod (AuthUserPassId (upId userPass)))
                          return (Right (authId, upId userPass))

changePasswordPage :: (Happstack m, MonadRoute m, URL m ~ AuthURL) =>
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
                            do e <- happstackEitherForm (R.form actionURL) "cpf" (changePasswordForm authStateH userPass)
                               case e of
                                 (Left formHtml) ->
                                    do r <- appTemplate "Change Passowrd" mempty $
                                                 H.div ! A.id "happstack-authenticate" $
                                                     do h1 $ do "Change password for "
                                                                toHtml $ unUserName $ upName userPass
                                                        formHtml
                                       ok r
                                 (Right passwd) ->
                                    do hashedPass <- liftIO $ mkHashedPass passwd
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

changePasswordForm  :: (Functor v, MonadIO v) => AcidState AuthState -> UserPass -> AuthForm v Text
changePasswordForm authStateH userPass =
    divHorizontal $
     fieldset $
      oldPassword *> newPassword <* changeBtn
    where
      -- form elements
      oldPassword =
          errorList ++>
          (divControlGroup $ label' "old password: " ++> divControls inputPassword) `transformEitherM` checkAuth

      checkAuth password =
              do r <- query' authStateH (CheckUserPass (unUserName $ upName userPass) password)
                 case r of
                   (Left e)  -> return (Left $ UPE e)
                   (Right _) -> return (Right password)

      password1, password2 :: (Functor v, MonadIO v) => AuthForm v Text
      password1 = divControlGroup $ label' "new password: "         ++> divControls inputPassword
      password2 = divControlGroup $ label' "new confirm password: " ++> divControls inputPassword

      newPassword :: (Functor v, MonadIO v) => AuthForm v Text
      newPassword =
          errorList ++>
           (((((,) <$> password1 <*> password2)) `transformEither` samePassword) `transformEither` minLength 6)

      samePassword :: (Text, Text) -> Either AuthTemplateError Text
      samePassword (p1, p2) =
              if p1 /= p2
               then (Left $ PasswordMismatch)
               else (Right p1)

      changeBtn :: (Functor v, MonadIO v) => AuthForm v (Maybe Text)
      changeBtn = divFormActions $ inputSubmit' "change" -- li $ mapView (\html -> html ! A.class_  "submit") $ inputSubmit "change"

minLength :: Int -> Text -> Either AuthTemplateError Text
minLength n s =
          if Text.length s >= n
          then (Right s)
          else (Left $ MinLength n)


divControlGroup :: (Functor m, MonadIO m) => AuthForm m a -> AuthForm m a
divControlGroup = mapView (\html -> H.div ! class_ "control-group" $ html)

divControls :: (Functor m, MonadIO m) => AuthForm m a -> AuthForm m a
divControls = mapView (\html -> H.div ! class_ "controls" $ html)

label' :: (Functor m, MonadIO m) => String -> AuthForm m ()
label' str = mapView (\html -> html ! class_"control-label") (R.label str)

divHorizontal :: (Functor m, MonadIO m) => AuthForm m a -> AuthForm m a
divHorizontal = mapView (\html -> H.div ! class_ "form-horizontal" $ html)

divInline :: (Functor m, MonadIO m) => AuthForm m a -> AuthForm m a
divInline = mapView (\html -> H.div ! class_ "form-inline" $ html)

divFormActions :: (Functor m, MonadIO m) => AuthForm m a -> AuthForm m a
divFormActions = mapView (\html -> H.div ! class_ "form-actions" $ html)

inputSubmit' :: (Functor m, MonadIO m) => Text -> AuthForm m (Maybe Text)
inputSubmit' str = mapView (\html -> html ! class_ "btn") (R.inputSubmit str)

{-
inputSubmit' str = inputSubmit str `setAttrs` [("class":="btn")]
inputCheckboxLabel lbl b =
    mapView (\xml -> [<label class="checkbox"><% xml %><% lbl %></label>])
                (inputCheckbox b)

label' str       = (label str `setAttrs` [("class":="control-label")])

labelCB str      = label str `setAttrs` [("class":="checkbox")]
--  divInline        = mapView (\xml -> [<div class="checkbox inline"><% xml %></div>])
divFormActions   = mapView (\xml -> [<div class="form-actions"><% xml %></div>])
divHorizontal    = mapView (\xml -> [<div class="form-horizontal"><% xml %></div>])
divControlGroup  = mapView (\xml -> [<div class="control-group"><% xml %></div>])
divControls      = mapView (\xml -> [<div class="controls"><% xml %></div>])
-}