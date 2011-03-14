{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Happstack.Auth.HSP.FormPart 
    ( formPart
    ) where

import Control.Applicative (Alternative, optional)
import Control.Monad
import Control.Monad.Trans
import qualified Data.Text       as Text
import           Data.Text       (Text)
import Happstack.Server           
import HSP                        (XMLGenT(..), Attr(..), EmbedAsAttr(..), EmbedAsChild(..), genElement, mapXMLGenT)
import Happstack.Server.HSX () -- instance (ServerMonad XMLGenT)
import qualified HSX.XMLGenerator as HSX
import Text.Digestive
import Text.Digestive.HSP.Html4

-- ^ turn a formlet into XML+ServerPartT which can be embedded in a larger document


formPart ::
  (EmbedAsChild m xml, EmbedAsAttr m (Attr String String), Functor m, MonadIO m, ToMessage b, FilterMonad Response m, WebMonad Response m, MonadPlus m, ServerMonad m, HasRqData m, Alternative m) 
  => String   -- ^ prefix
    -> String             -- ^ url to POST form results to
  -> (a -> XMLGenT m b) -- ^ handler used when form validates
  -> ([(FormRange, e)] -> [XMLGenT m (HSX.XML m)] -> XMLGenT m b) -- ^ handler used when form does not validate
  -> Form (XMLGenT m) Input e xml a      -- ^ the formlet
  -> XMLGenT m (HSX.XML m)
formPart prefix action handleSuccess handleFailure form =
    msum [ do methodM [GET, HEAD]
              (v, _) <- runForm form prefix NoEnvironment
              <form action=action method="POST" enctype="multipart/form-data" accept-charset="UTF-8">
               <% unView v [] %>
               </form>
         , do methodM POST
              mapXMLGenT (escape . fmap toResponse) $
                       do (v,r) <- runForm form prefix $ Environment (\i -> XMLGenT $ optional $ lookInput (showFormId i))
                          case r of
                            (Ok a)    -> handleSuccess a
                            (Error e) ->
                                handleFailure e [ <form action=action method="POST" enctype="multipart/form-data" accept-charset="UTF-8">
                                                   <% unView v e %>
                                                  </form>
                                                ]
                    
         ]
