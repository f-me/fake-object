
module Fake.Dictionary where

import Data.Text (Text)
import Data.Typeable

import Fake.Object


class Typeable d => Dictionary d where
  dictKey :: d -> ObjId "ident"
  dictKey _ = ObjId
  dictVal :: d -> Field "value" Text "value"
  dictVal _ = Field


class (Dictionary d, Dictionary (Parent d)) => NestedDictionary d where
  type Parent d
  dictParent :: d -> Field "parent" (Ident (Parent d)) "parent"
  dictParent _ = Field


class Dictionary d => SortedDictionary d where
  dictOrder :: d -> Field "order" Int "order"
  dictOrder _ = Field

