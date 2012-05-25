{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, TemplateHaskell #-}
module LongPoll where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad.Trans (liftIO)
import Data.Aeson (ToJSON, encode, toJSON)
import Data.Aeson.TH (deriveJSON)
import Data.ByteString.Char8 (unpack)
import Data.Map (Map)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Map as Map
import Happstack.Server
import Language.Javascript.JMacro
import System.Entropy (getEntropy)
import qualified Data.ByteString.Base64.URL as Base64

newtype PollId = PollId { unPollId :: String }
    deriving (Eq, Ord, Show, ToJExpr)

newtype PollMap a = PollMap { unPollMap :: TVar (Map PollId (ThreadId, TChan (PollData a))) }

initPolling :: IO (PollMap a)
initPolling =
    atomically $ PollMap <$> newTVar Map.empty

forkPoll :: PollMap a
         -> (TChan (PollData a) -> IO ())
         -> IO PollId
forkPoll (PollMap pm) proc =
    do pid <- PollId . unpack . Base64.encode <$> getEntropy 8
       tc  <- atomically $ newTChan
       tid <- forkIO $ proc tc
       atomically $ modifyTVar pm (\m -> Map.insert pid (tid, tc) m)
       return pid

pollUpdate :: (Happstack m, ToJSON a) =>
              PollMap a
           -> m Response
pollUpdate pm =
    do pid <- PollId <$> look "pollId"
       pollUpdate' pm pid


pollUpdate' :: (Happstack m, ToJSON a) =>
              PollMap a
           -> PollId
           -> m Response
pollUpdate' (PollMap pm) pid =
    do m <- liftIO $ atomically $ readTVar pm
       case Map.lookup pid m of
         Nothing -> notFound $ toResponse ("Invalid PollId: " ++ show pid)
         (Just (_, tc)) ->
             do a <- liftIO $ atomically $ readTChan tc
                ok $ toResponseBS (fromString "application/json") (encode a)

data PollData a
    = PollData { action :: String
               , value  :: a
               }
$(deriveJSON id ''PollData)

clientLoop :: String -- ^ url to POST requests to
           -> JExpr   -- ^ expression to apply to JSON value.
           -> PollId -- ^ PollId
           -> JStat
clientLoop url f pid =
    [jmacro|
             function longPoll () {
                            jQuery.post(`(url)`, { 'pollId' : `(pid)` }, function(d) { `(f)`(d.value); if (d.action == 'continue') longPoll(); }, 'json');
                          }
             $(document).ready(longPoll);
           |]

