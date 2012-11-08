{-# LANGUAGE NoImplicitPrelude #-}
module Language.Fay.HTML where

import Language.Fay.FFI
import Language.Fay.Prelude
import Language.Fay.JQuery

createElement :: String -> Fay Element
createElement = ffi "document.createElement(%1)"

data HTML
    = Element String [(String, String)] [HTML]
    | CDATA Bool String

genElement :: String
           -> [Fay (String, String)]
           -> [Fay HTML]
           -> Fay HTML
genElement n genAttrs genChildren =
    do attrs    <- sequence $ genAttrs
       children <- sequence $ genChildren
       return (Element n attrs children)

-- | render the 'HTML' into a JQuery DOM tree. You still need to
-- append the result somewhere.
renderHTML :: HTML
           -> Fay JQuery
renderHTML (Element n attrs children) =
    do elem <- selectElement =<< createElement n
       mapM_ (\(n, v) -> setAttr n v elem) attrs
       mapM_ (\child ->
                  do cElem <- renderHTML child
                     append cElem elem) children
       return elem

------------------------------------------------------------------------------
-- HTML Combinators

tr :: [Fay (String, String)] -> [Fay HTML] -> Fay HTML
tr = genElement "tr"

td :: [Fay (String, String)] -> [Fay HTML] -> Fay HTML
td = genElement "td"

span :: [Fay (String, String)] -> [Fay HTML] -> Fay HTML
span = genElement "span"

pcdata :: String -> Fay HTML
pcdata = return . CDATA True


