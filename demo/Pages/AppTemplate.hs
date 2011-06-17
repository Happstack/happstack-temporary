{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Pages.AppTemplate where

import Control.Applicative
import Control.Monad.Trans
import HSP (XMLGenT(..), EmbedAsChild(..), EmbedAsAttr(..), Attr(..), XML, genElement, genEElement, unXMLGenT)
import HSP.Google.Analytics (analytics)
import Happstack.Server
import Happstack.Server.HSP.HTML ()
import Happstack.Server.HSX ()
import HSP (XMLGenerator)
import qualified HSX.XMLGenerator as HSX
import SiteURL
import Web.Routes
import Web.Routes.XMLGenT
import Web.Routes.Happstack

appTemplate' :: 
    ( Happstack m
    , XMLGenerator m
    , EmbedAsChild m headers 
    , EmbedAsChild m body
    , EmbedAsAttr  m (Attr String String)
    )
    => String -- ^ title 
    -> headers  -- ^ extra tags to include in \<head\>
    -> body    -- ^ contents to put inside \<body\> 
    -> XMLGenT m (HSX.XML m)
appTemplate' title headers body = do 
   <html>
     <head>
       <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
       <% headers %>
       <title><% title %></title>
     </head>
     <body>
      <% body %>
     </body>
    </html>

appTemplate :: 
    ( Happstack m
    , ToMessage (HSX.XML m)
    , XMLGenerator m
    , EmbedAsChild m headers 
    , EmbedAsChild m body
    , EmbedAsAttr  m (Attr String String)
    )
    => String -- ^ title 
    -> headers  -- ^ extra tags to include in \<head\>
    -> body    -- ^ contents to put inside \<body\> 
    -> m Response
appTemplate title headers body =
    toResponse <$> (unXMLGenT (appTemplate' title headers body))
