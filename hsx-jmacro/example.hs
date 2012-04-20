{-# LANGUAGE FlexibleInstances, QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Main where

import Language.Javascript.JMacro
import HSX.JMacro
import HSP
import HSP.ServerPartT
import Happstack.Server
import Happstack.Server.HSP.HTML
import Happstack.Server.JMacro
import Data.Unique
import Control.Monad.Trans

instance IntegerSupply (ServerPartT IO) where
    nextInteger = fmap (fromIntegral . (`mod` 1024) . hashUnique) (liftIO newUnique)

main :: IO ()
main =
    do let html :: DOMNode
           html = <p>Generate javascript from <span class="foo" id="h">HTML & XML</span></p>

           js :: JStat
           js = [jmacro| document.getElementById('main').appendChild(`(html)`);
                         document.getElementById('main').appendChild(document.createTextNode("i like <em>em</em>"));
                       |]

           handler :: ServerPart XML
           handler = defaultTemplate "js-example" ()
                       <%>
                         <div id="main" />
                         <% js %>
                       </%>

       simpleHTTP nullConf handler
