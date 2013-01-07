
module Fake.Dictionary where

import Data.Text (Text)
import Data.Typeable

import Fake.Object


class Typeable d => Dictionary d where
  dictKey :: d -> Field "ident" (Ident d) "key"
  dictKey _ = Field
  dictVal :: d -> Field "value" Text "value"
  dictVal _ = Field
  dictFields :: [SomeField d]
  dictFields  = [SomeField dictKey, SomeField dictVal]


class (Dictionary d, Dictionary (Parent d)) => NestedDictionary d where
  type Parent d
  dictParent :: d -> Field "parent" (Ident (Parent d)) "parent"
  dictParent _ = Field
  nestedDictFields :: [SomeField d]
  nestedDictFields  = [SomeField dictParent]


class Dictionary d => SortedDictionary d where
  dictOrder :: d -> Field "order" Int "order"
  dictOrder _ = Field
  sortedDictFields :: [SomeField d]
  sortedDictFields  = [SomeField dictOrder]

