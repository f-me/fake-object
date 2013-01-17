
{-# LANGUAGE OverlappingInstances #-}

module Fake.Object.Internals.Bag where

import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Dynamic
import GHC.TypeLits
import Control.Lens

import Fake.Object.Internals.Field
import Fake.Object.Internals.Untyped


newtype Bag (fs :: [(*,Symbol,*)]) = Bag (Map Text Dynamic)


instance MapWithTag (Bag fs) where
  toUntyped (Bag m) = m
  fromUntyped = Bag


instance (SingI name, Typeable typ)
  => HasField (Bag ('(cls,name,typ) ': fs))  (cls -> Field name typ desc)
  where
    type FType (Bag ('(cls,name,typ) ': fs)) (cls -> Field name typ desc) = typ
    fld = mapLens . Text.pack . fieldName

instance
  (SingI name, Typeable typ
  ,HasField (Bag fs) (cls -> Field name typ desc)
  )
  => HasField (Bag ('(c,n,t) ': fs))  (cls -> Field name typ desc)
  where
    type FType (Bag ('(c,n,t) ': fs)) (cls -> Field name typ desc) = typ
    fld = mapLens . Text.pack . fieldName
