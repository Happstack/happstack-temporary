{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Main where

import Control.Monad
import Happstack.Server
import Happstack.Server.HSP.HTML
import HSP
import HSP.ServerPartT

main :: IO ()
main = simpleHTTP nullConf $ msum [ dir "fay" $ serveFile (asContentType "text/javascript") "hsx-client.js"
                                  , (fmap toResponse (unXMLGenT page))
                                  ]

page :: XMLGenT (ServerPartT IO) XML
page =
    <html>
     <head>
      <script src="http://code.jquery.com/jquery-1.9.1.min.js"></script>
      <script type="text/javascript" src="/fay"></script>
      <style type="text/css">.red { color: red; }</style>
     </head>
     <body>
      <h1>Test.</h1>
      <div id="thediv">
       <p>This paragraph is in the original HTML.</p>
      </div>
     </body>
    </html>

