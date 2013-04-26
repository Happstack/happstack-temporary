-- | functions for using Heist with Happstack
--
-- See the Heist Section of the Happstack Crash Course for detailed documentation:
--
--  <http://happstack.com/docs/crashcourse/>
module Happstack.Server.Heist
    ( initHeistCompiled
    , heistServe
    )
    where

import Blaze.ByteString.Builder                (toLazyByteString)
import Control.Monad                           (MonadPlus(mzero), msum, liftM)
import Control.Monad.Trans                     (MonadIO(liftIO))
import Control.Monad.Trans.Either              (EitherT(runEitherT))
import Data.Monoid                             (mempty)
import           Data.ByteString.Char8         (ByteString)
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy          as L
import Data.Text                               (Text)
import Happstack.Server                        (Happstack, Response, ServerMonad, askRq, nullDir, rqPaths, toResponseBS)
import Happstack.Server.FileServe.BuildingBlocks (combineSafe)
import Heist                                   (AttrSplice, HeistConfig(..), HeistState, MIMEType, defaultLoadTimeSplices, initHeist, loadTemplates)
import Heist.Compiled                          (Splice, renderTemplate)
import System.FilePath                         (joinPath)

initHeistCompiled :: (MonadIO m, Monad n) =>
                     [(Text, Splice n)]     -- ^ compiled splices
                  -> [(Text, AttrSplice n)] -- ^ attribute splices
                  -> FilePath               -- ^ path to template directory
                  -> m (Either [String] (HeistState n))
initHeistCompiled splices attrSplices templateDir =
    liftIO $ runEitherT $
           do templateRepo <- loadTemplates templateDir
              initHeist $ mempty { hcLoadTimeSplices  = defaultLoadTimeSplices
                                 , hcCompiledSplices  = splices
                                 , hcAttributeSplices = attrSplices
                                 , hcTemplates        = templateRepo
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
