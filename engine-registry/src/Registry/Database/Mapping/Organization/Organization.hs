module Registry.Database.Mapping.Organization.Organization where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Registry.Model.Organization.Organization
import Shared.Database.Mapping.Common

instance ToField OrganizationRole where
  toField = toFieldGenericEnum

instance FromField OrganizationRole where
  fromField = fromFieldGenericEnum

instance ToRow Organization

instance FromRow Organization
