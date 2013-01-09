
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Fake.Object.Internals
  (Object(..)
  ,Field(..)
  ,ObjId(..)
  ,fld
  ,Ident(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word
import Data.Dynamic
import GHC.TypeLits
import Control.Lens


-- Object is just a map from field name to field value (where value is wrapped
-- with `Dynamic`).
-- Object is tagged by class type to allow typesafe operations.
-- In order to guaranty type safety only `Object` type is exported (without
-- constructor).
newtype Object cls = Object {untyped :: Map Text Dynamic}
  -- `Object` is `Typeable` instance so we can use it as a value of a field in
  -- another object
  deriving Typeable


-- These are internal functions to get/set object fields.
-- FIXME: Use real EXCEPTIONS here
objGet
  :: Typeable typ
  => Object cls -> Text -> typ
objGet obj@(Object o) f
  = case Map.lookup f o of
    Nothing -> error $ info ++ ": no such field"
    Just v  -> case fromDynamic v of
      Nothing -> error $ info ++ ": invalid field type " ++ show (dynTypeRep v)
      Just x  -> x
  where
    info = "objGet {field: " ++ show f ++ "}"


objSet
  :: Typeable typ
  => Object cls -> Text -> typ -> Object cls
objSet (Object o) f v =  Object $ Map.insert f (toDyn v) o


objLens :: Typeable typ => Text -> SimpleLens (Object cls) typ
objLens field = lens (`objGet` field) (`objSet` field)



data ObjId (name :: Symbol) = ObjId
data Field (name :: Symbol) typ (desc :: Symbol) = Field

class Typeable (Res cls f) => FieldLens cls f where
  type Res cls f
  fld :: (cls -> f) -> SimpleLens (Object cls) (Res cls f)

instance (Typeable cls, SingI name) => FieldLens cls (ObjId name) where
  type  Res cls (ObjId name) = Ident cls
  fld _ = objLens $ Text.pack $ fromSing (sing :: Sing name)

instance (Typeable typ, SingI name)
  => FieldLens cls (Field name typ desc)
  where
    type Res cls (Field name typ desc) = typ
    fld _ = objLens $ Text.pack $ fromSing (sing :: Sing name)


newtype Ident cls = Ident {identValue :: Word64}
  deriving (Show, Ord, Eq, Typeable)
