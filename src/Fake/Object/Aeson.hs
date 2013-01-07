
{-# LANGUAGE ScopedTypeVariables #-}
module Fake.Object.Aeson where

import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Aeson.Types hiding (Object)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as H

import Data.Dynamic
import GHC.TypeLits

import Fake.Object.Internals as Fake


-- FIXME: maybe `Ident cls` should be represented as "clsName:1234"
--        not just as "1234"?
deriving instance FromJSON (Ident cls)



type FakeO = Map Text Dynamic


mkFromJSON
  :: IterateFromJSON cls f
  => f -> (Value -> Parser (Fake.Object cls))
mkFromJSON f = \v -> case v of
  Aeson.Object o -> iterateFromJSON f o Map.empty
  _ -> fail $ "iterateFromJSON: object expected " ++ show v


class IterateFromJSON cls f where
  iterateFromJSON :: f -> Aeson.Object -> FakeO -> Parser (Fake.Object cls)


instance IterateFromJSON cls cls where
  iterateFromJSON _ o res
    | H.null o = pure $ Fake.Object res
    | otherwise = fail $ "iterateFromJSON: unexpected fields " ++ show o


instance (IterateFromJSON cls res, SingI name, Typeable typ, FromJSON typ)
  => IterateFromJSON cls (Field name typ desc -> res)
  where
    iterateFromJSON f o res = case H.lookup fieldName o of
      Nothing  -> fail $ "iterateFromJSON: missing field " ++ show fieldName
      Just val -> parseJSON val
        >>= \(v :: typ) -> iterateFromJSON
          (f Field)
          (H.delete fieldName o)
          (Map.insert fieldName (toDyn v) res)
      where
        fieldName = Text.pack $ fromSing (sing :: Sing name)


