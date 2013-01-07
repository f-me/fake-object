
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}

module Carma.Dictionaries where


import Data.Text (Text)
import Data.Dynamic

import Fake.Object


class Typeable d => Dictionary d where
  dictKey :: d -> Field d "ident" (Ident d) "key"
  dictKey _ = Field
  dictVal :: d -> Field d "value" Text "value"
  dictVal _ = Field
  dictFields :: [SomeField d]
  dictFields  = [SomeField $ dictKey undefined, SomeField $ dictVal undefined]


class (Dictionary d, Typeable d, Dictionary (Parent d), Typeable (Parent d))
  => NestedDictionary d
  where
    type Parent d

    dictParent :: d -> Field d "parent" (Ident (Parent d)) "parent"
    dictParent _ = Field

    nestedDictFields :: [SomeField d]
    nestedDictFields  = [SomeField $ dictParent undefined]


class (Dictionary d, Typeable d) => SortedDictionary d where
  dictOrder :: d -> Field d "order" Int "order"
  dictOrder _ = Field
  sortedDictFields :: [SomeField d]
  sortedDictFields  = [SomeField $ dictOrder undefined]



data CarMake deriving Typeable
instance Dictionary CarMake
instance Model CarMake where modelFields = dictFields

data CarModel deriving Typeable
instance Dictionary CarModel
instance NestedDictionary CarModel where type Parent CarModel = CarMake
instance Model CarModel where modelFields = dictFields ++ nestedDictFields


data ObjBase = ObjBase
  {ctime   :: Field ObjBase "ctime" String "Дата создания объекта"
  ,mtime   :: Field ObjBase "mtime" String "Дата последнего изменения объекта"
  ,deleted :: Field ObjBase "deleted" Bool "Флажок архивности"
  }
  deriving Typeable

instance Model ObjBase where modelFields = getModelFields ObjBase

data Case = Case
  {obj      :: Field Case "obj"      (Object ObjBase)         ""
  ,carMake  :: Field Case "carMake"  (Maybe (Ident CarMake))  "Марка автомобиля"
  ,carModel :: Field Case "carModel" (Maybe (Ident CarModel)) "Модель автомобиля"
  }
  deriving Typeable

instance Model Case where modelFields = getModelFields Case
