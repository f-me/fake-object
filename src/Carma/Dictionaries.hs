

module Carma.Dictionaries where

import Data.Typeable
import Fake.Object
import Fake.Dictionary


data CarMake
deriving instance Typeable CarMake
instance Dictionary CarMake
instance Model CarMake where modelFields = dictFields

data CarModel
deriving instance Typeable CarModel
instance Dictionary CarModel
instance NestedDictionary CarModel where type Parent CarModel = CarMake
instance Model CarModel where modelFields = dictFields ++ nestedDictFields


data ObjBase = ObjBase
  {ctime   :: Field "ctime" String "Дата создания объекта"
  ,mtime   :: Field "mtime" String "Дата последнего изменения объекта"
  ,deleted :: Field "deleted" Bool "Флажок архивности"
  }
  deriving Typeable

instance Model ObjBase where modelFields = getModelFields ObjBase

data Case = Case
  {obj      :: Field "obj"      (Object ObjBase)         ""
  ,carMake  :: Field "carMake"  (Maybe (Ident CarMake))  "Марка автомобиля"
  ,carModel :: Field "carModel" (Maybe (Ident CarModel)) "Модель автомобиля"
  }
  deriving Typeable

instance Model Case where modelFields = getModelFields Case
