{-# LANGUAGE FlexibleInstances, TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Main where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad.Trans
import Data.Aeson.TH
import Data.Unique
import Happstack.Server
import Happstack.Server.HSP.HTML
import HSP
import HSP.ServerPartT
import HSX.JMacro (IntegerSupply(..), nextInteger')
import Language.Javascript.JMacro
import LongPoll

instance IntegerSupply (ServerPartT IO) where
    nextInteger = fmap (fromIntegral . (`mod` 1024) . hashUnique) (liftIO newUnique)

data Msg = Msg { message :: String }

$(deriveJSON id ''Msg)

main =
    do pm <- initPolling
       simpleHTTP nullConf (route pm)


route :: PollMap Msg -> ServerPart Response
route pm =
    do decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
       msum [ dir "jquery.js" $ serveFile (asContentType "text/javascript") "/usr/share/javascript/jquery/jquery.min.js"
            , nullDir >> counter pm
            , dir "update" $ pollUpdate pm
            ]


counter :: PollMap Msg
        -> ServerPart Response
counter pm =
    do pid <- liftIO $ forkPoll pm counterProc
       toResponse <$>
         defaultTemplate "counter" <%><script type="text/javascript" src="jquery.js"></script><% clientLoop "update" callback pid %></%>
           <%>
             <h1>Messages</h1>
             <ol id="msgs"></ol>
           </%>
    where
      callback =
          [jmacroE| function(d) { $("#msgs").append($(document.createElement('li')).append(d.message)); } |]

counterProc :: TChan (PollData Msg)
            -> IO ()
counterProc tchan =
    do mapM_ (\s -> do threadDelay 2000000 ; atomically $ writeTChan tchan (PollData "continue" (Msg s))) ["one","two","three","four"]
       threadDelay 2000000
       atomically $ writeTChan tchan (PollData { action = "stop"
                                               , value  = Msg "five"
                                               })
