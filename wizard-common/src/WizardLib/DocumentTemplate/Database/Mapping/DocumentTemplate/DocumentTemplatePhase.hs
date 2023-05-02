module WizardLib.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplatePhase where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Common.Database.Mapping.Common
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

instance ToField DocumentTemplatePhase where
  toField = toFieldGenericEnum

instance FromField DocumentTemplatePhase where
  fromField = fromFieldGenericEnum
