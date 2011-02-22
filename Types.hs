{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeSynonymInstances, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators, RecordWildCards
    #-}
module Types where

import Control.Applicative
import Data.Data
import Happstack.Data
import Happstack.Server
import HSP
import Web.Routes
import Web.Routes.XMLGenT
import Web.Routes.Happstack
import Web.Routes.MTL

newtype UserId = UserId { unUserId :: Integer }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
instance Version UserId
$(deriveSerialize ''UserId)
$(deriveNewData [''UserId])

instance PathInfo UserId where
    toPathSegments (UserId i) = toPathSegments i
    fromPathSegments = UserId <$> fromPathSegments

succUserId :: UserId -> UserId
succUserId (UserId i) = UserId (succ i)

instance EmbedAsAttr (RouteT url (ServerPartT IO)) (Attr String url) where
    asAttr (n := u) = 
        do url <- showURL u
           asAttr $ MkAttr (toName n, pAttrVal url)

instance (Happstack m) => Happstack (RouteT url m)