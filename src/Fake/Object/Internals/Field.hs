
{-# LANGUAGE RankNTypes #-}

module Fake.Object.Internals.Field where

import GHC.TypeLits
import Control.Lens


data ObjId (name :: Symbol) = ObjId
data Field (name :: Symbol) typ (desc :: Symbol) = Field

class HasField obj fld where
  type FType obj fld
  fld :: fld -> SimpleLens obj (FType obj fld)
