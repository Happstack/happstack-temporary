{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, TemplateHaskell, TypeFamilies, TypeOperators #-}
{-|

This app demonstrates basic usage of the happstack-authenticate
library. Note that a majority of the app is just boring boilerplate
for parsing command-line arguments.

-}
module Main where

import Acid                              (Acid(..), withAcid)
import Control.Applicative               ((<$>))
import Control.Concurrent                (forkIO, killThread)
import Control.Monad                     (msum, mzero)
import Control.Monad.Trans               (liftIO)
import Data.Acid.Advanced                (query')
import Data.Text                         (Text)
import qualified Data.Text               as Text
import Happstack.Server                  ( Happstack, Request(rqSecure, rqUri), Response(..), ServerPartT
                                         , BodyPolicy(..), Conf(..), askRq, decodeBody
                                         , defaultBodyPolicy, dir, nullDir, ok, nullConf
                                         , seeOther, simpleHTTP, toResponse)
import Pages.AppTemplate                 (appTemplate)
import Pages.Home                        (homePage)
import Happstack.Auth.Core.Auth          (AskAuthState(..))
import Happstack.Auth.Blaze.Templates    (handleAuthProfile, authProfileHandler)
import Happstack.Server.SimpleHTTP       (waitForTermination)
import Happstack.Server.SimpleHTTPS      (simpleHTTPS, nullTLSConf, TLSConf(..))
import ProfileData                       (handleProfileData)
import System.Console.GetOpt             (ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt, usageInfo)
import System.Environment                (getArgs, getProgName)
import System.Exit                       (exitFailure)
import System.FilePath                   ((</>))

------------------------------------------------------------------------------
-- Server Configuration
------------------------------------------------------------------------------

data ServerConfig = ServerConfig
    { scHostname   :: String
    , scPort       :: Int
    , scTLSPort    :: Int
    , scBodyPolicy :: BodyPolicy
    }

defaultConfig :: ServerConfig
defaultConfig = ServerConfig
    { scHostname   = "localhost"
    , scPort       = 8000
    , scTLSPort    = 8443
    , scBodyPolicy = defaultBodyPolicy "/tmp" 0 1000 1000
    }

------------------------------------------------------------------------------
-- base uri calculations
------------------------------------------------------------------------------

httpBaseURI :: ServerConfig -> String
httpBaseURI ServerConfig{..} =
    "http://" ++ scHostname ++ ":" ++ (show scPort) ++ "/"

httpsBaseURI :: ServerConfig -> String
httpsBaseURI ServerConfig{..} =
    "https://" ++ scHostname ++ ":" ++ (show scTLSPort) ++ "/"

------------------------------------------------------------------------------
-- Command line processing
------------------------------------------------------------------------------

data Option
    = Hostname String
    | Port     Int
    | TLSPort  Int
    | Help
      deriving (Eq, Ord, Read, Show)

optDescr :: [OptDescr Option]
optDescr =
    [ Option [] ["hostname"] (ReqArg Hostname "hostname" ) "External hostname name of this server."
    , Option [] ["port"]     (ReqArg (Port . read) "port" ) "Port for HTTP service."
    , Option [] ["tls-port"] (ReqArg (Port . read) "port" ) "Port for HTTPS service."
    ]

applyOptions :: [Option] -> ServerConfig -> ServerConfig
applyOptions options config = foldr applyOption config options
    where
      applyOption (Hostname hn) c = c { scHostname = hn }
      applyOption (Port n)      c = c { scPort     = n  }
      applyOption (TLSPort n)   c = c { scTLSPort  = n  }

parseOptions :: ServerConfig -> IO ServerConfig
parseOptions config =
    do args <- getArgs
       case getOpt Permute optDescr args of
         (opts, _, []) -> return (applyOptions opts config)
         (_,_,errs)    -> do name <- getProgName
                             let errMsg = unlines errs
                             let desc = errMsg ++ "\n\n" ++ name ++ " --hostname localhost --port 8000 --tls-port 8443"
                             putStrLn $ usageInfo desc optDescr
                             exitFailure

------------------------------------------------------------------------------
-- main
------------------------------------------------------------------------------

main :: IO ()
main =
    do sc@ServerConfig{..} <- parseOptions defaultConfig
       withAcid Nothing $ \acid ->
           do tidHTTP  <- forkIO $ simpleHTTP (nullConf { port = scPort }) (route acid sc)
              let tlsConf = nullTLSConf { tlsPort = scTLSPort
                                        , tlsCert = "ssl/test.crt"
                                        , tlsKey  = "ssl/test.key"
                                        }
              tidHTTPS <- forkIO $ simpleHTTPS tlsConf (routeTLS acid sc)
              putStrLn "Server listening on:"
              putStrLn $ httpBaseURI sc
              putStrLn $ httpsBaseURI sc
              waitForTermination
              killThread tidHTTP
              killThread tidHTTPS

-- | map an incoming 'Request' a handler
route :: Acid -- ^ database handle
      -> ServerConfig
      -> ServerPartT IO Response
route acid@Acid{..} serverConfig = do
  do decodeBody (scBodyPolicy serverConfig)
     msum [ dir "web" $ do secure <- rqSecure <$> askRq
                           if secure
                              then mzero
                              else do u <- rqUri <$> askRq
                                      seeOther (httpsBaseURI serverConfig ++ (dropWhile (== '/') u)) (toResponse ())
          , dir "dump_auth" $ do authState <- query' acidAuth AskAuthState
                                 ok $ toResponse (show authState)
          , nullDir >> homePage acid
          ]

-- | map an incoming 'Request' a handler
routeTLS :: Acid -- ^ database handle
         -> ServerConfig
         -> ServerPartT IO Response
routeTLS acid@Acid{..} serverConfig =
    do decodeBody (scBodyPolicy serverConfig)
       let baseURI = Text.pack $ httpsBaseURI serverConfig
       msum [ authProfileHandler baseURI (Text.pack "web") acidAuth acidProfile appTemplate Nothing (Just baseURI) (Text.pack "/profile_data/new")
            , handleProfileData acidAuth acidProfile acidProfileData
            , route acid serverConfig
            ]
