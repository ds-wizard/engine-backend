module Wizard.Database.Mapping.ActionKey.ActionKeyType where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Common.Database.Mapping.Common
import Wizard.Model.ActionKey.ActionKeyType

instance ToField ActionKeyType where
  toField = toFieldGenericEnum

instance FromField ActionKeyType where
  fromField = fromFieldGenericEnum
