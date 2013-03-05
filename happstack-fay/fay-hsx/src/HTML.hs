{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}
{- |

A simple library for client-side HTML generation. Compatible with hsx2hs.

-}
module HTML where

import Prelude
import JQuery
import FFI

type Text = String

data Attr a b = a := b


-- | type class for embedding values as HTML children
--
-- since Fay does not yet support type-class methods we have to fake it via the ffi
class AsChild a             -- where asChild :: a -> Fay HTML
instance AsChild [Char]     -- where asChild s = return (CDATA True s)
instance AsChild (Fay HTML) -- where asChild = id

-- and here is the scary ffi code.
asChild :: (AsChild a) => a -> Fay HTML
asChild =
    ffi "(function () { if (%1 instanceof Fay$$Cons) { return { 'instance' : 'CDATA', slot1 : true, slot2 : Fay$$fayToJs_string(%1) }; } else { var monad = Fay$$_(%1, true); return monad.value; }})()"

class AsAttr a                       -- where asAttr :: a -> Fay (String, String)
instance AsAttr (Attr String String) -- where asAttr (a := b) = return (a, b)

asAttr :: (Attr String String) -> Fay (String, String)
asAttr ((:=) a b) = return (a, b)

-- | ADT for 'HTML'
data HTML
    = Element String [(String, String)] [HTML] -- ^ Element name attributes children
    | CDATA Bool String                        -- ^ CDATA needEscaping value

-- | generate an HTML element
genElement :: (Maybe String, String)  -- ^ Element name
           -> [Fay (String, String)]  -- ^ list of attributes
           -> [Fay HTML]              -- ^ list of children
           -> Fay HTML
genElement (_, n) genAttrs genChildren =
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
renderHTML (CDATA True str) =
    selectElement =<< createTextNode str
renderHTML (CDATA False str) =
    do alert "Unsure how to insert pre-escaped text into the generated HTML."
       selectElement =<< createTextNode str

-- | Alert using window.alert.
alert :: String -> Fay ()
alert = ffi "window.alert(%1)"

------------------------------------------------------------------------------
-- HTML Combinators

-- | \<tr\>
tr :: [Fay (String, String)] -- ^ attributes
   -> [Fay HTML]             -- ^ children
   -> Fay HTML
tr ats chd = genElement (Nothing, "tr") ats chd

-- | \<td\>
td :: [Fay (String, String)]  -- ^ attributes
   -> [Fay HTML]              -- ^ children
   -> Fay HTML
td = genElement (Nothing, "td")

-- | \<span\>
span_ :: [Fay (String, String)] -- ^ attributes
     -> [Fay HTML]             -- ^ children
     -> Fay HTML
span_ = genElement (Nothing, "span")

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

-- | create a new text node
--
-- NOTE: this doesn't really return an Element. It returns a TextNode
-- or something. But fay-jquery only supports the Element type...
createTextNode :: String -- ^ text to insert in the node
               -> Fay Element
createTextNode = ffi "document.createTextNode(%1)"
