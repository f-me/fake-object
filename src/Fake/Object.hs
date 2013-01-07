
module Fake.Object
  (Object
  ,untyped
  ,Field(..)
  ,SomeField(..)
  ,fld
  ,Ident
  ,Model(..)
  ,getModelFields
  ) where

import Data.Map (Map)
import Data.Dynamic

import Fake.Object.Internals
