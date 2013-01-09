
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fake.Object.Aeson
  (mkFromJSON
  ) where

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

instance (IterateFromJSON cls res, Typeable cls, SingI name)
  => IterateFromJSON cls (ObjId name -> res)
  where
    iterateFromJSON = iterateFromJSON'
      (undefined :: Ident cls)
      (Text.pack $ fromSing (sing :: Sing name))

instance (IterateFromJSON cls res, SingI name, Typeable typ, FromJSON typ)
  => IterateFromJSON cls (Field name typ desc -> res)
  where
    iterateFromJSON = iterateFromJSON'
      (undefined :: typ)
      (Text.pack $ fromSing (sing :: Sing name))


iterateFromJSON'
  :: (FromJSON typ, Typeable typ
     ,IterateFromJSON cls (f -> res), IterateFromJSON cls res)
  => typ -> Text -> (f -> res) -> Aeson.Object -> FakeO
  -> Parser (Fake.Object cls)
iterateFromJSON' (_ :: typ) fieldName f o res
  = case H.lookup fieldName o of
    Nothing  -> fail $ "iterateFromJSON: missing field " ++ show fieldName
    Just val -> parseJSON val
      >>= \(v :: typ) -> iterateFromJSON
        (f undefined)
        (H.delete fieldName o)
        (Map.insert fieldName (toDyn v) res)
