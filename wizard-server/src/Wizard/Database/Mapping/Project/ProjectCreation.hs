module Wizard.Database.Mapping.Project.ProjectCreation where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Common.Database.Mapping.Common
import Wizard.Model.Tenant.Config.TenantConfig

instance ToField ProjectCreation where
  toField = toFieldGenericEnum

instance FromField ProjectCreation where
  fromField = fromFieldGenericEnum
