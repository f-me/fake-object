
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Fake.Object.Internals.Field where

import GHC.TypeLits
import Control.Lens

data FReq = Req | Opt
data Field (req :: FReq) (name :: Symbol) typ (desc :: Symbol) = Field

fieldName :: SingI name => (cls -> Field rq name typ desc) -> String
fieldName (_ :: cls -> Field rq name typ desc)
  = fromSing (sing :: Sing name)

class HasField obj fld where
  type FldType obj fld
  fld :: fld -> Lens' obj (FldType obj fld)
