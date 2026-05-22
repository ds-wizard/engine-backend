module Registry.Database.Mapping.UserEmailLink.UserEmailLinkType where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Registry.Model.UserEmailLink.UserEmailLinkType
import Shared.Common.Database.Mapping.Common

instance ToField UserEmailLinkType where
  toField = toFieldGenericEnum

instance FromField UserEmailLinkType where
  fromField = fromFieldGenericEnum
