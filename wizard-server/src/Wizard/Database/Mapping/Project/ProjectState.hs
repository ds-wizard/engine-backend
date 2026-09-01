module Wizard.Database.Mapping.Project.ProjectState where

import Database.PostgreSQL.Simple.FromField

import Shared.Common.Database.Mapping.Common
import Wizard.Model.Project.ProjectState

instance FromField KnowledgeModelProjectState where
  fromField = fromFieldGenericEnum

instance FromField DocumentTemplateProjectState where
  fromField = fromFieldGenericEnum
