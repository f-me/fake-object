
{-# LANGUAGE OverlappingInstances #-}

module Fake.Object
  (toUntyped
  ,Field(..)
  ,FReq(..)
  ,fieldName
  ,HasField(..) -- FIXME: export only `fld`?
  ,Object
  ,Ident
  ,Bag
  ,newObj
  ,NewObjFields(..)
  ) where

import Fake.Object.Internals.Untyped
import Fake.Object.Internals.Field
import Fake.Object.Internals.Object
import Fake.Object.Internals.Ident
import Fake.Object.Internals.Bag

import qualified Data.Map as Map
import GHC.TypeLits


newObj
  :: NewObjFields cls f
  => f -> (Bag cls (ReqFields cls f) -> Bag cls (ReqFields cls f)) -> Object cls
newObj _ g = Object . toUntyped . g $ Bag Map.empty

class NewObjFields cls f where
  type ReqFields cls f :: [(Symbol,*)]

instance NewObjFields cls fs => NewObjFields cls (Field Req n t d -> fs)
  where
    type ReqFields cls (Field Req n t d -> fs) = '(n,t) ': ReqFields cls fs

instance NewObjFields cls fs => NewObjFields cls (Field Opt n t d -> fs)
  where
    type ReqFields cls (Field Opt n t d -> fs) = ReqFields cls fs
