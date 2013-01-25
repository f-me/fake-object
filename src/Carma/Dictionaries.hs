
{-# LANGUAGE EmptyDataDecls #-}

module Carma.Dictionaries where

import Control.Lens
import Data.Typeable
import Data.Aeson hiding (Object)

import Fake.Object
import Fake.Object.Aeson
import Fake.Object.DDL
-- import Fake.Dictionary


data CarMake
deriving instance Typeable CarMake
-- instance Dictionary CarMake

data CarModel
deriving instance Typeable CarModel
-- instance Dictionary CarModel
-- instance NestedDictionary CarModel where type Parent CarModel = CarMake


data ObjBase = ObjBase
  {ctime   :: Field Req "ctime" String "Дата создания объекта"
  ,mtime   :: Field Req "mtime" String "Дата последнего изменения объекта"
  ,deleted :: Field Req "deleted" Bool "Флажок архивности"
  }
  deriving Typeable

-- instance FromJSON (Object ObjBase) where parseJSON = mkFromJSON ObjBase
-- instance SqlFields ObjBase where sqlFields = getSqlFields ObjBase

data Case = Case
  {ident    :: Field Req "ident"    (Ident Case)     "ident"
  ,obj      :: Field Req "obj"      (Object ObjBase) ""
  ,carMake  :: Field Opt "carMake"  (Ident CarMake)  "Марка автомобиля"
  ,carModel :: Field Opt "carModel" (Ident CarModel) "Модель автомобиля"
  }
  deriving Typeable

-- Модель описывается типом данных с одним конструктором
-- Сам тип будет тегом в типе объектов
data Test = Test
  {field1   :: Field Opt "field1"   Int "Тестовое поле 1"
  ,field2   :: Field Req "field2"   Int "Тестовое поле 2"
  }
  deriving Typeable

instance NewObjFields Test Test where
  type ReqFields Test Test = '[]

-- Чтобы создать объект, нужно указать значения для всех его обязательных
-- полей. Другие поля в этом же месте задавать (к сожалению) нельзя.
test :: Object Test
test = newObj Test
  ((fld field2 .~ 123)
  .(fld field1 .~ Just 123)
  )

-- instance FromJSON (Object Case) where parseJSON = mkFromJSON Case
-- instance SqlFields Case where sqlFields = getSqlFields Case

