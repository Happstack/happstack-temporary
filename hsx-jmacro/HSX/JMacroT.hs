{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies, QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module HSX.JMacroT where

import Control.Applicative (Applicative, Alternative)
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.RWS
import Control.Monad.Cont
import HSX.XMLGenerator
import qualified HSX.XMLGenerator as HSX
import Language.Javascript.JMacro

-- | isomorphic to IdentityT, but used for generating javascript that generates XML/HTML
newtype JMacroT m a = JMacroT { unJMacroT :: m a }
    deriving ( Functor, Applicative, Alternative, Monad, MonadIO, MonadPlus, MonadState s, MonadReader r, MonadWriter w, MonadRWS r w s, MonadCont, MonadError e)

instance MonadTrans JMacroT where
    lift = JMacroT

mapJMacroT :: (m a -> n b) -> JMacroT m a -> JMacroT n b
mapJMacroT f (JMacroT ma) = JMacroT (f ma)

evalJMacroT :: XMLGenT (JMacroT m) JExpr -> m JExpr
evalJMacroT = unJMacroT . unXMLGenT

type JMacroM = JMacroT Identity

evalJMacroM :: XMLGenT JMacroM JExpr -> JExpr
evalJMacroM = runIdentity . unJMacroT . unXMLGenT

instance ToJExpr (XMLGenT JMacroM JExpr) where
    toJExpr = evalJMacroM

instance (Functor m, Monad m) => XMLGen (JMacroT m) where
    type XML (JMacroT m)          = JExpr
    newtype Child (JMacroT m)     = JMChild { unJMChild :: JExpr }
    newtype Attribute (JMacroT m) = JMAttr  { unJMAttr  :: JExpr }
    genElement    = element
    xmlToChild    = JMChild
    pcdataToChild str = JMChild $ [jmacroE| document.createTextNode(`(str)`) |]

element :: (Functor m, Monad m, EmbedAsAttr (JMacroT m) attr, EmbedAsChild (JMacroT m) child) => Name -> [attr] -> [child] -> XMLGenT (JMacroT m) JExpr
element (ns, nm) attrs childer =
    do ats      <- fmap (map unJMAttr  . concat) $ mapM asAttr  attrs
       children <- fmap (map unJMChild . concat) $ mapM asChild childer
       return
        [jmacroE| (function { var node = `(createElement ns nm)`;
                              `(map (setAttributeNode node) ats)`;
                              `(map (appendChild node) children)`;
                            return node;
                          })()
         |]
      where
        createElement Nothing n = [jmacroE| document.createElement(`(n)`) |]
        createElement (Just ns) n = [jmacroE| document.createElementNS(`(ns)`, `(n)`) |]
        appendChild node c =
            [jmacroE| `(node)`.appendChild(`(c)`) |]
        setAttributeNode node attr =
            [jmacroE| `(node)`.setAttributeNode(`(attr)`) |]

instance (Functor m, Monad m) => EmbedAsAttr (JMacroT m) (Attr String String) where
    asAttr (n := v) =
        return [JMAttr [jmacroE| (function (){ var attrNode = document.createAttribute(`(n)`);
                                               attrNode.nodeValue = `(v)`;
                                               return attrNode;
                                             })()
                       |]]
