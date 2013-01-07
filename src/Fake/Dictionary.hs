
module Fake.Dictionary where

import Data.Text (Text)
import Data.Typeable

import Fake.Object


class Typeable d => Dictionary d where
  dictKey :: d -> Field d "ident" (Ident d) "key"
  dictKey _ = Field
  dictVal :: d -> Field d "value" Text "value"
  dictVal _ = Field
  dictFields :: [SomeField d]
  dictFields  = [SomeField $ dictKey undefined, SomeField $ dictVal undefined]


class (Dictionary d, Dictionary (Parent d)) => NestedDictionary d where
  type Parent d
  dictParent :: d -> Field d "parent" (Ident (Parent d)) "parent"
  dictParent _ = Field
  nestedDictFields :: [SomeField d]
  nestedDictFields  = [SomeField $ dictParent undefined]


class Dictionary d => SortedDictionary d where
  dictOrder :: d -> Field d "order" Int "order"
  dictOrder _ = Field
  sortedDictFields :: [SomeField d]
  sortedDictFields  = [SomeField $ dictOrder undefined]

