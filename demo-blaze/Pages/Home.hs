{-# LANGUAGE RecordWildCards, OverloadedStrings, TypeFamilies #-}
module Pages.Home where

import Acid (Acid(..))
import Control.Monad.Trans (liftIO)
import Data.Acid (query)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import qualified Data.Text as Text
import Happstack.Server (Happstack, Response, ServerPartT)
import Pages.AppTemplate (appTemplate)
import Happstack.Auth (AuthProfileURL(..), AuthURL(..), getUserId)
import ProfileData (AskProfileData(..), profileMsg)
import Text.Blaze.Html5            as H hiding (fieldset, ol, li, label, head)
import Text.Blaze.Html5.Attributes as A hiding (label)

-- | function which generates the homepage
homePage :: Acid -- ^ database handle
         -> ServerPartT IO Response
homePage Acid{..} =
    do mUserId <- getUserId acidAuth acidProfile
       case mUserId of
         Nothing -> do
             let loginURL = "/web/auth/login"
             appTemplate "not logged in." mempty $
               H.div $ H.p $ "You can login " >> (H.a ! href loginURL $ "here.")
         (Just uid) -> do 
             mpd <- liftIO $ query acidProfileData (AskProfileData uid)
             let logoutURL  = "/web/auth/logout"
                 addAuthURL = "/web/auth/add_auth"
             appTemplate "logged in." mempty $
               H.div $ do
                 H.p $ "You are logged in as" >> toHtml (show uid)
                 H.p $ "You can logout " >> (H.a ! href logoutURL $ "here") >> "."
                 H.p $ "You can add an additional auth method ">> (H.a ! href addAuthURL $ "here") >> "."
                 H.p $ "Your message is: " >> (toHtml $ fromMaybe (Text.pack "profile data missing.") (fmap profileMsg mpd))
