-- | This module provides support for serving compiled Heist templates using Happstack.
--
-- The primary function provided by this module is:
--
--    heistServe :: (Happstack m) => HeistState m -> m Response
--
-- It also provides the 'initHeistCompiled' helper function for
-- creating a 'HeistState'. Though you are free to use other functions
-- from the Heist library instead.
--
-- Here is a simple example:
--
-- > module Main where
-- >
-- > import Control.Applicative    ((<$>))
-- > import Control.Monad          (msum)
-- > import qualified Data.Text    as T
-- > import Happstack.Server       (dir, nullConf, nullDir, simpleHTTP, seeOther, toResponse)
-- > import Happstack.Server.Heist (heistServe, initHeistCompiled)
-- > import Heist                  (getParamNode)
-- > import Heist.Compiled         (Splice, yieldRuntimeText)
-- > import qualified Text.XmlHtml as X
-- >
-- > -- | factorial splice
-- > factSplice :: (Monad m) => Splice m
-- > factSplice =
-- >     do intStr <- T.unpack . X.nodeText <$> getParamNode
-- >        let res = yieldRuntimeText $
-- >                    do case reads intStr of
-- >                         [(n,[])] ->
-- >                             return (T.pack $ show $ product [1..(n :: Integer)])
-- >                         _ ->
-- >                             return (T.pack $ "Unable to parse " ++ intStr ++ " as an Integer.")
-- >        return $ res
-- >
-- > main :: IO ()
-- > main =
-- >   do heistState <- do
-- >        r <- initHeistCompiled [(T.pack "fact", factSplice)] [] "."
-- >        case r of
-- >          (Left e) -> error $ unlines e
-- >          (Right heistState) -> return $ heistState
-- >      simpleHTTP nullConf $ msum
-- >        [ dir "heist" $ heistServe heistState
-- >        , nullDir >> seeOther "/heist/factorial" (toResponse "/heist/factorial")
-- >        ]
--
-- It uses the following template file (@factorial.tpl@):
--
-- @
-- \<html\>
--   \<head\>
--     \<title\>Factorial Page\<\/title\>
--   \<\/head\>
--   \<body\>
--     \<h1\>Factorial Page\<\/h1\>
--     \<p\>The factorial of 6 is \<fact\>6\<\/fact\>\<\/p\>
--   \<\/body\>
-- \<\/html\>
-- @
--
-- For more information on using Compiled Heist Templates see:
--
-- <http://snapframework.com/docs/tutorials/compiled-splices>
--
-- And also see the Heist Section of the Happstack Crash Course:
--
--  <http://happstack.com/docs/crashcourse/>
--
module Happstack.Server.Heist
    ( initHeistCompiled
   , heistServe
    )
    where

import Blaze.ByteString.Builder                (toLazyByteString)
import Control.Monad                           (MonadPlus(mzero), msum)
import Control.Monad.Trans                     (MonadIO(liftIO))
import Control.Monad.Trans.Either              (EitherT(runEitherT))
import Data.Monoid                             (mempty)
import           Data.ByteString.Char8         (ByteString)
import qualified Data.ByteString.Char8         as B
import Data.Text                               (Text)
import Happstack.Server                        (Happstack, Response, ServerMonad, askRq, nullDir, rqPaths, toResponseBS)
import Happstack.Server.FileServe.BuildingBlocks (combineSafe)
import Heist                                   (AttrSplice, HeistConfig(..), HeistState, defaultLoadTimeSplices, initHeist, loadTemplates)
import Heist.Compiled                          (Splice, renderTemplate)
import System.FilePath                         (joinPath)

initHeistCompiled :: (MonadIO m, Monad n) =>
                     [(Text, Splice n)]     -- ^ compiled splices
                  -> [(Text, AttrSplice n)] -- ^ attribute splices
                  -> FilePath               -- ^ path to template directory
                  -> m (Either [String] (HeistState n))
initHeistCompiled splices attrSplices templateDir =
    liftIO $ runEitherT $
              initHeist $ mempty { hcLoadTimeSplices  = defaultLoadTimeSplices
                                 , hcCompiledSplices  = splices
                                 , hcAttributeSplices = attrSplices
                                 , hcTemplateLocations = [loadTemplates templateDir]
                                 }

heistServe :: (Happstack m) =>
              HeistState m
           -> m Response
heistServe heistState =
    msum [ nullDir >> renderHeistTemplate heistState (B.pack "index")
         , do rq <- askRq
              case combineSafe "" (joinPath (rqPaths rq)) of
                Nothing -> mzero
                (Just safepath) ->
                    renderHeistTemplate heistState (B.pack safepath)
         ]

renderHeistTemplate :: (MonadPlus n) =>
                       HeistState n
                    -> ByteString
                    -> n Response
renderHeistTemplate heistState templateName =
       case renderTemplate heistState templateName of
         Nothing -> mzero
         Just (builderM, mimeType) ->
             do builder <- builderM
                return $ toResponseBS mimeType (toLazyByteString builder)
