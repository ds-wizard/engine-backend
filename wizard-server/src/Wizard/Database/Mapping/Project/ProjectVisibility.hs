module Wizard.Database.Mapping.Project.ProjectVisibility where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Common.Database.Mapping.Common
import Wizard.Model.Project.Project

instance ToField ProjectVisibility where
  toField = toFieldGenericEnum

instance FromField ProjectVisibility where
  fromField = fromFieldGenericEnum
