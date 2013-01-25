
{-# LANGUAGE RankNTypes #-}

module Fake.Object.Internals.Untyped
  (MapWithTag(..)
  ,mapLens
  ) where


import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Dynamic
import Control.Lens


class MapWithTag obj where
  fromUntyped :: Map Text Dynamic -> obj
  toUntyped :: obj -> Map Text Dynamic

mapGet
  :: (MapWithTag obj, Typeable typ)
  => obj -> Text -> typ
mapGet obj fld
  = case Map.lookup fld $ toUntyped obj of
    Nothing -> error $ info ++ ": no such field"
    Just v  -> case fromDynamic v of
      Nothing -> error $ info ++ ": invalid field type " ++ show (dynTypeRep v)
      Just x  -> x
  where
    info = "mapGet {field: " ++ show fld ++ "}"

mapSet
  :: (MapWithTag obj, Typeable typ)
  => obj -> Text -> typ -> obj
mapSet obj fld val
  = fromUntyped
  $ Map.insert fld (toDyn val)
  $ toUntyped obj

mapLens
  :: (MapWithTag obj, Typeable typ)
  => Text -> Lens' obj typ
mapLens fld = lens (`mapGet` fld) (`mapSet` fld)
