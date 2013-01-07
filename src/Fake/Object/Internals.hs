
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Fake.Object.Internals
  (Object(..)
  ,Field(..)
  ,fld
  ,Ident(..)
  ,SomeField(..)
  ,Model(..)
  ,getFields
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
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
  deriving (Typeable, Show)


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



data Field (name :: Symbol) typ (desc :: Symbol) = Field

-- TODO: class FieldLens
fld
  :: (SingI name, Typeable typ)
  => (cls -> Field name typ desc) -> SimpleLens (Object cls) typ
fld (_ :: cls -> Field name typ desc)
  = objLens $ Text.pack $ fromSing (sing :: Sing name)


-- FIXME: Later we can change `Text` to `Word64`
newtype Ident cls = Ident {identValue :: Text}
  deriving (Show, Ord, Eq, Typeable)


data SomeField cls
  = forall name typ desc . (SingI name, Typeable typ, SingI desc)
  => SomeField (cls -> Field name typ desc)


class Model cls where
  modelFields :: [SomeField cls]

class    EnumFields cls f   where getFields :: f -> [SomeField cls]
instance EnumFields cls cls where getFields _ = []
instance (SingI name, Typeable typ, SingI desc, EnumFields cls res)
  => EnumFields cls (Field name typ desc -> res)
  where
    getFields f
      = SomeField (const Field :: cls -> Field name typ desc)
      : getFields (f Field)

