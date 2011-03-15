{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Pages.Logout where

import Control.Applicative(Alternative(..))
import Pages.AppTemplate
import Happstack.Auth.Core.Auth
import Happstack.Auth.Core.AuthURL
import Happstack.Server
import HSP
import qualified HSX.XMLGenerator as HSX
import Web.Routes
import Web.Routes.Happstack

logoutPage :: (XMLGenerator m, Alternative m, Happstack m, ShowURL m, URL m ~ AuthURL, EmbedAsAttr m (Attr String AuthURL)) => XMLGenT m (HSX.XML m)
logoutPage =
    do deleteAuthCookie
       <p>You are now logged out. Click <a href=A_Login>here</a> to log in again.</p>
       
