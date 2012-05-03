{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies, TypeSynonymInstances, QuasiQuotes #-}
-- | This experimental module provides a monad transformer 'JMacroT'
-- and corresponding 'XMLGenerator' instance which can be used to
-- directly generate javascript which builds an XML/HTML DOM.
--
-- This is similar to the 'ToJExpr XMLToDOM' instance except that
-- there is no intermediate XML type. The 'XMLGenerator' instance
-- directly generates the javascript needed to build the DOM.
--
-- This is intellectually fun. But it is not clear how it is valuable.
-- That is why this module is marked as experimental.
module HSX.JMacroT
    ( JMacroT(..)
    , evalJMacroT
    , mapJMacroT
    , JMacroM
    , evalJMacroM
    ) where

import Control.Applicative        (Applicative, Alternative)
import Control.Monad              (MonadPlus)
import Control.Monad.Cont         (MonadCont)
import Control.Monad.Identity     (Identity(..))
import Control.Monad.Error        (MonadError)
import Control.Monad.Reader       (MonadReader)
import Control.Monad.State        (MonadState)
import Control.Monad.Writer       (MonadWriter)
import Control.Monad.RWS          (MonadRWS)
import Control.Monad.Trans        (MonadIO, MonadTrans(..))
import qualified Data.Text        as Strict
import qualified Data.Text.Lazy   as Lazy
import HSX.XMLGenerator           (Attr(..), XMLGen(..), XMLGenT(..), XMLGenerator, AppendChild(..), EmbedAsAttr(..), EmbedAsChild(..), Name(..), SetAttr(..), unXMLGenT)

import Language.Javascript.JMacro (ToJExpr(..), JExpr(..), JStat(..), JVal(JVar), Ident(StrI), ToStat(..), jmacroE, jLam, jVarTy)

-- | isomorphic to IdentityT, but used for generating javascript that generates XML/HTML
newtype JMacroT m a = JMacroT { unJMacroT :: m a }
    deriving ( Functor, Applicative, Alternative, Monad, MonadIO, MonadPlus, MonadState s, MonadReader r, MonadWriter w, MonadRWS r w s, MonadCont, MonadError e)

instance MonadTrans JMacroT where
    lift = JMacroT

-- | map a function over the inner monad
mapJMacroT :: (m a -> n b) -> JMacroT m a -> JMacroT n b
mapJMacroT f (JMacroT ma) = JMacroT (f ma)

-- | unwrap the 'XMLGenT' and 'JMacroT' constructors
evalJMacroT :: XMLGenT (JMacroT m) JExpr -> m JExpr
evalJMacroT = unJMacroT . unXMLGenT

-- | an alias for 'JMacroT Identity'
type JMacroM = JMacroT Identity

-- | evaluate 'JMacroM'
evalJMacroM :: XMLGenT JMacroM a -> a
evalJMacroM = runIdentity . unJMacroT . unXMLGenT

instance (ToJExpr a) => ToJExpr (XMLGenT JMacroM a) where
    toJExpr = toJExpr . evalJMacroM

instance (Functor m, Monad m) => XMLGen (JMacroT m) where
    type XMLType          (JMacroT m) = JExpr
    newtype ChildType     (JMacroT m) = JMChild { unJMChild :: JExpr }
    newtype AttributeType (JMacroT m) = JMAttr  { unJMAttr  :: JExpr }
    genElement        = element
    xmlToChild        = JMChild
    pcdataToChild str = JMChild $ [jmacroE| document.createTextNode(`(str)`) |]


-- | generate an XML Element
element :: (Functor m, Monad m, EmbedAsAttr (JMacroT m) attr, EmbedAsChild (JMacroT m) child) =>
           Name    -- ^ element name
        -> [attr]  -- ^ attributes
        -> [child] -- ^ children
        -> XMLGenT (JMacroT m) JExpr
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

-- | javascript to create an element
createElement Nothing n = [jmacroE| document.createElement(`(n)`) |]
createElement (Just ns) n = [jmacroE| document.createElementNS(`(ns)`, `(n)`) |]

-- | javascript to append a child to an element
appendChild :: JExpr -> JExpr -> JExpr
appendChild node c =
  [jmacroE| `(node)`.appendChild(`(c)`) |]

-- | javascript to set the attribute node of an element
setAttributeNode :: JExpr -> JExpr -> JExpr
setAttributeNode node attr =
    [jmacroE| `(node)`.setAttributeNode(`(attr)`) |]

instance (Functor m, Monad m) => EmbedAsAttr (JMacroT m) (Attr String String) where
    asAttr (n := v) =
        return [JMAttr [jmacroE| (function (){ var attrNode = document.createAttribute(`(n)`)
                                             ; attrNode.nodeValue = `(v)`
                                             ; return attrNode;
                                             })()
                       |]]

instance (Functor m, Monad m) => EmbedAsChild (JMacroT m) Char where
    asChild c = return [pcdataToChild [c]]

instance (Functor m, Monad m) => EmbedAsChild (JMacroT m) String where
    asChild str = return [pcdataToChild str]

instance (Functor m, Monad m) => EmbedAsChild (JMacroT m) Strict.Text where
    asChild txt = return [JMChild $ [jmacroE| document.createTextNode(`(Strict.unpack txt)`) |]]

instance (Functor m, Monad m) => EmbedAsChild (JMacroT m) Lazy.Text where
    asChild txt = return [JMChild $ [jmacroE| document.createTextNode(`(Lazy.unpack txt)`) |]]

instance (Functor m, Monad m) => EmbedAsChild (JMacroT m) () where
    asChild () = return []

instance (Functor m, Monad m) => EmbedAsAttr (JMacroT m) (Attr String Bool) where
    asAttr (n := True)  = asAttr (n := "true")
    asAttr (n := False) = asAttr (n := "false")

instance (Functor m, Monad m) => EmbedAsAttr (JMacroT m) (Attr String Int) where
    asAttr (n := v) = asAttr (n := show v)

instance (Functor m, Monad m) => AppendChild (JMacroT m) JExpr where
    appChild parent child =
        do c <- child
           return $ [jmacroE| appendChild parent (unJMChild c) |]
    appAll parent children =
        do chs <- children
           return $ [jmacroE| `(map (appendChild parent) (map unJMChild chs))` |]

instance (Functor m, Monad m) => SetAttr (JMacroT m) JExpr where
    setAttr elem attrNode =
        do a <- attrNode
           return $ [jmacroE| `(setAttributeNode elem (unJMAttr a))` |]
    setAll elem attrNodes =
        do as <- attrNodes
           return $ [jmacroE| `(map (setAttributeNode elem) (map unJMAttr as))` |]

instance (Functor m, Monad m) => XMLGenerator (JMacroT m)
