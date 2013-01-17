
module Fake.Object.Internals.Ident where

import Data.Word
import Data.Typeable


newtype Ident cls = Ident Word64
  deriving Typeable
