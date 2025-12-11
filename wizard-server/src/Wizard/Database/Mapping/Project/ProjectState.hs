module Wizard.Database.Mapping.Project.ProjectState where

import Database.PostgreSQL.Simple.FromField

import Shared.Common.Database.Mapping.Common
import Wizard.Model.Project.ProjectState

instance FromField ProjectState where
  fromField = fromFieldGenericEnum
