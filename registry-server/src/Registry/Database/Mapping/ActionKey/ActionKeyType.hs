module Registry.Database.Mapping.ActionKey.ActionKeyType where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Registry.Model.ActionKey.ActionKeyType
import Shared.Common.Database.Mapping.Common

instance ToField ActionKeyType where
  toField = toFieldGenericEnum

instance FromField ActionKeyType where
  fromField = fromFieldGenericEnum
