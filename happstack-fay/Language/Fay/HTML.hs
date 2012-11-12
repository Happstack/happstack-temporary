{-# LANGUAGE NoImplicitPrelude #-}
{- |

A simple library for client-side HTML generation.

-}
module Language.Fay.HTML where

import Language.Fay.FFI
import Language.Fay.Prelude
import Language.Fay.JQuery


-- | ADT for 'HTML'
data HTML
    = Element String [(String, String)] [HTML] -- ^ Element name attributes children
    | CDATA Bool String                        -- ^ CDATA needEscaping value

-- | generate an HTML element
genElement :: String                  -- ^ Element name
           -> [Fay (String, String)]  -- ^ list of attributes
           -> [Fay HTML]              -- ^ list of children
           -> Fay HTML
genElement n genAttrs genChildren =
    do attrs    <- sequence $ genAttrs
       children <- sequence $ genChildren
       return (Element n attrs children)

-- | render the 'HTML' into a JQuery DOM tree. You still need to
-- append the result somewhere.
--
-- NOTE: This function requires 'jQuery'
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

-- | \<tr\>
tr :: [Fay (String, String)] -- ^ attributes
   -> [Fay HTML]             -- ^ children
   -> Fay HTML
tr = genElement "tr"

-- | \<td\>
td :: [Fay (String, String)]  -- ^ attributes
   -> [Fay HTML]              -- ^ children
   -> Fay HTML
td = genElement "td"

-- | \<span\>
span :: [Fay (String, String)] -- ^ attributes
     -> [Fay HTML]             -- ^ children
     -> Fay HTML
span = genElement "span"

-- | create a text node from the 'String'. The 'String' will be
-- automatically escaped.
pcdata :: String -> Fay HTML
pcdata = return . CDATA True

------------------------------------------------------------------------------
-- HTML Combinators

-- | create a new 'Element'
createElement :: String -- ^ name of the element
              -> Fay Element
createElement = ffi "document.createElement(%1)"
