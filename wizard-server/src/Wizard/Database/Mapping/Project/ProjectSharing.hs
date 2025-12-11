module Wizard.Database.Mapping.Project.ProjectSharing where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Common.Database.Mapping.Common
import Wizard.Model.Project.Project

instance ToField ProjectSharing where
  toField = toFieldGenericEnum

instance FromField ProjectSharing where
  fromField = fromFieldGenericEnum
