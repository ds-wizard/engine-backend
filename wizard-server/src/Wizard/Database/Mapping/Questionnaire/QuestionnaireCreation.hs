module Wizard.Database.Mapping.Questionnaire.QuestionnaireCreation where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Common.Database.Mapping.Common
import Wizard.Model.Tenant.Config.TenantConfig

instance ToField QuestionnaireCreation where
  toField = toFieldGenericEnum

instance FromField QuestionnaireCreation where
  fromField = fromFieldGenericEnum
