module Main where

import Control.Applicative ((<$>))
import Control.Concurrent  (forkIO)
import Control.Monad       (msum)
import Control.Monad.Trans (liftIO)
import Happstack.Server    (Request(rqSecure), Response, ServerPart, askRq, dir, ok, notFound, toResponse, simpleHTTP, nullConf)
import Happstack.Server.SimpleHTTPS (TLSConf(..), simpleHTTPS, nullTLSConf)
import System.Random       (randomRIO)


tlsConf :: TLSConf
tlsConf =
    nullTLSConf { tlsPort = 8443
                , tlsCert = "ssl/test.crt"
                , tlsKey  = "ssl/test.key"
                }

routes :: ServerPart Response
routes =
    msum [ dir "favicon.ico" $ 
               notFound $ toResponse "sorry, no favicon.ico"
         , do r <- liftIO $ randomRIO (1,100 :: Int)
              b <- rqSecure <$> askRq
              let secure | b == True = "Connection secure."
                         | otherwise = "Connection insecure."
              ok $ toResponse $ secure ++ " Your random number is: " ++ show r
         ]

main :: IO ()
main = 
    do forkIO $ simpleHTTPS tlsConf  routes
       simpleHTTP           nullConf routes