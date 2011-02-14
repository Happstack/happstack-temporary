{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Pages.AppTemplate where

import Control.Applicative
import Control.Monad.Trans
import HSP (XMLGenT(..), EmbedAsChild(..), EmbedAsAttr(..), Attr(..), XML, genElement, genEElement, unXMLGenT)
import HSP.Google.Analytics (analytics)
import Happstack.State
import Happstack.Server
import Happstack.Server.HSP.HTML ()
import Happstack.Server.HSX ()
import HSP (XMLGenerator)
import SiteURL
import Web.Routes
import Web.Routes.XMLGenT
import Web.Routes.Happstack

appTemplate' :: 
    ( EmbedAsChild (RouteT url (ServerPartT IO)) headers 
    , EmbedAsChild (RouteT url (ServerPartT IO)) body
    , EmbedAsAttr  (RouteT url (ServerPartT IO)) (Attr String String)
    )
    => String -- ^ title 
    -> headers  -- ^ extra tags to include in \<head\>
    -> body    -- ^ contents to put inside \<body\> 
    -> XMLGenT (RouteT url (ServerPartT IO)) XML
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
    ( EmbedAsChild (RouteT url (ServerPartT IO)) headers 
    , EmbedAsChild (RouteT url (ServerPartT IO)) body
    , EmbedAsAttr  (RouteT url (ServerPartT IO)) (Attr String String)
    )
    => String -- ^ title 
    -> headers  -- ^ extra tags to include in \<head\>
    -> body    -- ^ contents to put inside \<body\> 
    -> RouteT url (ServerPartT IO) Response
appTemplate title headers body =
    toResponse <$> (unXMLGenT (appTemplate' title headers body))


-- | move to happstack?
queryJust :: (MonadIO m, WebMonad Response m, FilterMonad Response m, Show ev, QueryEvent ev (Maybe res)) => ev -> m res
queryJust ev =
    do mr <- query ev
       case mr of
         Nothing  -> escape $ internalServerError $ toResponse ("query returned Nothing: " ++ show ev)
         (Just r) -> return r

