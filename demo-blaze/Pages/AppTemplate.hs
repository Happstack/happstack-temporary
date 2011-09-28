{-# LANGUAGE OverloadedStrings #-}
module Pages.AppTemplate where

import Happstack.Server            (Response, toResponse)
import Text.Blaze.Html5            as H hiding (fieldset, ol, li, label, head)
import qualified Text.Blaze.Html5  as H
import Text.Blaze.Html5.Attributes as A hiding (label)

appTemplate' :: 
       String -- ^ title 
    -> Html  -- ^ extra tags to include in \<head\>
    -> Html    -- ^ contents to put inside \<body\> 
    -> Html
appTemplate' title headers body = do 
  H.html $ do
    H.head $ do
      H.meta ! httpEquiv "Content-Type" ! content "text/html; charset=UTF-8"
      headers
      H.title (toHtml title)
    H.body
     body

appTemplate :: 
    ( Monad m
    )
    => String -- ^ title 
    -> Html  -- ^ extra tags to include in \<head\>
    -> Html    -- ^ contents to put inside \<body\> 
    -> m Response
appTemplate title headers body =
    return $ toResponse (appTemplate' title headers body)
