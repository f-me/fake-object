
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


newtype Bag cls (fs :: [(Symbol,*)]) = Bag (Map Text Dynamic)


instance MapWithTag (Bag cls fs) where
  toUntyped (Bag m) = m
  fromUntyped = Bag

class Elem (x :: (Symbol,*)) (xs :: [(Symbol,*)])
instance Elem x (x ': xs)
instance Elem x xs => Elem x (y ': xs)


instance (SingI name, Typeable typ, Elem '(name,typ) fs)
  => HasField (Bag cls fs) (cls -> Field Req name typ desc)
  where
    type FldType (Bag cls fs) (cls -> Field Req name typ desc) = typ
    fld = mapLens . Text.pack . fieldName

instance (SingI name, Typeable typ, Elem '(name,typ) fs)
  => HasField (Bag cls fs) (cls -> Field Opt name typ desc)
  where
    type FldType (Bag cls fs) (cls -> Field Opt name typ desc) = Maybe typ
    fld = mapLens . Text.pack . fieldName
