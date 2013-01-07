

module Carma.Dictionaries where

import Data.Typeable
import Fake.Object
import Fake.Object.Aeson
import Fake.Dictionary

import Data.Aeson hiding (Object)

data CarMake
deriving instance Typeable CarMake
instance Dictionary CarMake

data CarModel
deriving instance Typeable CarModel
instance Dictionary CarModel
instance NestedDictionary CarModel where type Parent CarModel = CarMake


data ObjBase = ObjBase
  {ctime   :: Field "ctime" String "Дата создания объекта"
  ,mtime   :: Field "mtime" String "Дата последнего изменения объекта"
  ,deleted :: Field "deleted" Bool "Флажок архивности"
  }
  deriving Typeable

instance FromJSON (Object ObjBase) where parseJSON = mkFromJSON ObjBase

data Case = Case
  {obj      :: Field "obj"      (Object ObjBase)         ""
  ,carMake  :: Field "carMake"  (Maybe (Ident CarMake))  "Марка автомобиля"
  ,carModel :: Field "carModel" (Maybe (Ident CarModel)) "Модель автомобиля"
  }
  deriving Typeable


instance FromJSON (Object Case) where parseJSON = mkFromJSON Case
