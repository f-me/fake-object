
module Fake.Object
  (toUntyped
  ,Field(..)
  ,fieldName
  ,HasField(..) -- FIXME: export only `fld`?
  ,Object
  ,Ident
  ,Bag
  ) where

import Fake.Object.Internals.Untyped
import Fake.Object.Internals.Field
import Fake.Object.Internals.Object
import Fake.Object.Internals.Ident
import Fake.Object.Internals.Bag
