{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Pages.Home where

import Acid (Acid(..))
import Data.Acid.Advanced (query')
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import qualified Data.Text as Text
import Happstack.Server (Response, ServerPartT)
import Pages.AppTemplate (appTemplate)
import SiteURL (SiteURL(..))
import Happstack.Auth (AuthProfileURL(..), AuthURL(..), getUserId)
import ProfileData (AskProfileData(..), profileMsg)
import Web.Routes  (RouteT, showURL)
import Web.Routes.Happstack        ()
import Text.Blaze.Html5            as H hiding (fieldset, ol, li, label, head)
import Text.Blaze.Html5.Attributes as A hiding (label)

-- | function which generates the homepage
homePage :: Acid -- ^ database handle
         -> RouteT SiteURL (ServerPartT IO) Response
homePage Acid{..} =
    do mUserId <- getUserId acidAuth acidProfile
       case mUserId of
         Nothing -> 
             do loginURL <- showURL (U_AuthProfile $ AuthURL A_Login)
                appTemplate "not logged in." mempty $
                  H.div $ p  $ do "You can login "
                                  H.a ! href (toValue loginURL) $ "here."
         (Just uid) -> do 
             mpd <- query' acidProfileData (AskProfileData uid)
             logoutURL  <- showURL (U_AuthProfile $ AuthURL A_Logout)
             addAuthURL <- showURL (U_AuthProfile $ AuthURL A_AddAuth)
             appTemplate "logged in." mempty $
               H.div $ do
                 H.p $ "You are logged in as" >> toHtml (show uid)
                 H.p $ "You can logout " >> (H.a ! href (toValue logoutURL) $ "here") >> "."
                 H.p $ "You can add an additional auth method ">> (H.a ! href (toValue addAuthURL) $ "here") >> "."
                 H.p $ "Your message is: " >> (toHtml $ fromMaybe (Text.pack "profile data missing.") (fmap profileMsg mpd))
