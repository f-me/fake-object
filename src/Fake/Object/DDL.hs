
{-# LANGUAGE ScopedTypeVariables, OverlappingInstances #-}

module Fake.Object.DDL where

import GHC.TypeLits
import Data.Dynamic

import Fake.Object


mkDDL :: (Typeable cls, SqlFields cls) => cls -> String
mkDDL (_ :: cls) = show (tableName, fields)
  where
    tableName = tyConName $ typeRepTyCon $ typeOf (undefined :: cls)
    fields = sqlFields :: [SqlField cls]


data SqlField cls = SqlField
  {name :: String
  ,desc :: String
  ,typ  :: TypeRep
  }
  deriving Show


class SqlFields cls where sqlFields :: [SqlField cls]


class GetSqlFields cls f where
  getSqlFields :: f -> [SqlField cls]

instance GetSqlFields cls cls where
  getSqlFields _ = []

instance (GetSqlFields cls res, Typeable cls, SingI nm)
  => GetSqlFields cls (ObjId nm -> res)
  where
    getSqlFields f = SqlField fieldName fieldDesc fieldType : rest
      where
        fieldDesc = "primary key"
        fieldName = fromSing (sing :: Sing nm)
        fieldType = typeOf   (undefined :: Ident cls)
        rest      = getSqlFields (f ObjId)

instance (GetSqlFields cls res, SingI nm, SqlFields typ)
  => GetSqlFields cls (Field nm (Object typ) desc -> res)
  where
    getSqlFields f = map castF sqlFields ++ rest
      where
        fieldName = fromSing (sing :: Sing nm)
        rest      = getSqlFields (f Field)
        castF :: SqlField typ -> SqlField cls
        castF (SqlField n d t) = SqlField
          {name = fieldName ++ "_" ++ n
          ,desc = d
          ,typ  = t
          }

instance (GetSqlFields cls res, SingI nm, Typeable typ, SingI desc)
  => GetSqlFields cls (Field nm typ desc -> res)
  where
    getSqlFields f = SqlField fieldName fieldDesc fieldType : rest
      where
        fieldDesc = fromSing (sing :: Sing desc)
        fieldName = fromSing (sing :: Sing nm)
        fieldType = typeOf   (undefined :: typ)
        rest      = getSqlFields (f Field)

