
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Fake.Object.Internals.Field where

import GHC.TypeLits
import Control.Lens


data Field (name :: Symbol) typ (desc :: Symbol) = Field

fieldName :: SingI name => (cls -> Field name typ desc) -> String
fieldName (_ :: cls -> Field name typ desc)
  = fromSing (sing :: Sing name)

class HasField obj fld where
  type FType obj fld
  fld :: fld -> SimpleLens obj (FType obj fld)
