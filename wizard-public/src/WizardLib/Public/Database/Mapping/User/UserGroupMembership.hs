module WizardLib.Public.Database.Mapping.User.UserGroupMembership where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Common.Database.Mapping.Common
import WizardLib.Public.Model.User.UserGroupMembership

instance ToField UserGroupMembershipType where
  toField = toFieldGenericEnum

instance FromField UserGroupMembershipType where
  fromField = fromFieldGenericEnum

instance ToRow UserGroupMembership

instance FromRow UserGroupMembership
