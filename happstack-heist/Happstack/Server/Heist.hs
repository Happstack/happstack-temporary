-- | functions for using Heist with Happstack
--
-- See the Heist Section of the Happstack Crash Course for detailed documentation:
--
--  <http://happstack.com/docs/crashcourse/Templates.html#helloheist>
module Happstack.Server.Heist
    ( templateReloader
    , templateServe
    , render
    ) where

import Blaze.ByteString.Builder                (toLazyByteString)
import Control.Monad                           (MonadPlus(mzero), msum, liftM)
import Control.Monad.Trans                     (MonadIO(liftIO))
import           Data.ByteString.Char8         (ByteString)
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy          as L
import Happstack.Server                        (Response, ServerMonad, askRq, nullDir, rqPaths, toResponseBS)
import System.FilePath                         (joinPath)
import Heist.Compiled                          (renderTemplate)
import Heist.TemplateDirectory (TemplateDirectory, getDirectoryHS, reloadTemplateDirectory)

-- | serve the heist templates from the 'TemplateDirectory m'
templateServe :: (ServerMonad m, MonadPlus m, MonadIO m) =>
                 TemplateDirectory m
              -> m Response
templateServe td =
    msum [ nullDir >> render td (B.pack "index")
         , do rq <- askRq
              let safepath = joinPath $ filter (\x->not (null x) && x /= ".." && x /= ".") (rqPaths rq)
              render td (B.pack safepath)
         ]

-- | force a reload of the templates from disk
templateReloader :: (MonadIO m, MonadIO n) =>
                    TemplateDirectory m
                 -> n Response
templateReloader td = do
    e <- liftIO $ reloadTemplateDirectory td
    return $ toResponseBS (B.pack "text/plain; charset=utf-8") $
        L.fromChunks [either B.pack (const $ B.pack "Templates loaded successfully.") e]


-- | render the specified template
render:: (MonadPlus m, MonadIO m) =>
         TemplateDirectory m  -- ^ 'TemplateDirectory' handle
      -> ByteString           -- ^ template name
      -> m Response
render td template = do
    ts    <- liftIO $ getDirectoryHS td
    let t = renderTemplate ts template
    flip (maybe mzero) t $ \(builder, mimeType) ->
      liftM (toResponseBS mimeType . toLazyByteString) builder
