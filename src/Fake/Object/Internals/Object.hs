
module Fake.Object.Internals.Object where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Dynamic
import GHC.TypeLits

import Fake.Object.Internals.Field
import Fake.Object.Internals.Untyped


-- Object is just a map from field name to field value (where value is wrapped
-- with `Dynamic`).
-- Object is tagged by class type to allow typesafe operations.
-- In order to guaranty type safety only `Object` type is exported (without
-- constructor).
newtype Object cls = Object (Map Text Dynamic)
  -- `Object` is `Typeable` instance so we can use it as a value of a field in
  -- another object
  deriving Typeable


instance MapWithTag (Object cls) where
  toUntyped (Object o) = o
  fromUntyped = Object


instance (SingI name, Typeable typ)
  => HasField (Object cls) (cls -> Field Req name typ desc)
  where
    type FldType (Object cls) (cls -> Field Req name typ desc) = typ
    fld = mapLens . Text.pack . fieldName

instance (SingI name, Typeable typ)
  => HasField (Object cls) (cls -> Field Opt name typ desc)
  where
    type FldType (Object cls) (cls -> Field Opt name typ desc) = Maybe typ
    fld = mapLens . Text.pack . fieldName
