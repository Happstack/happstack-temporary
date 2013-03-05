{-# LANGUAGE NoImplicitPrelude, DeriveDataTypeable, PackageImports #-}
module ResponseType where

-- NOTE: this module is used by both fay and haskell code. But
-- @fay-base@ uses @ifdefs@ to ensure that each side sees what they
-- need.
import "fay-base" Data.Data
import "fay-base" Prelude

-- | 'ResponseType' is used in lieu of `GADTs` as a mechanism for
-- specifying the expected return type of remote AJAX calls.
data ResponseType a = ResponseType
    deriving (Data, Typeable, Read, Show, Eq)
