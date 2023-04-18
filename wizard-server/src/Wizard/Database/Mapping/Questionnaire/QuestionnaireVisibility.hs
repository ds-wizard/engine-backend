module Wizard.Database.Mapping.Questionnaire.QuestionnaireVisibility where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Common.Database.Mapping.Common
import Wizard.Model.Questionnaire.Questionnaire

instance ToField QuestionnaireVisibility where
  toField = toFieldGenericEnum

instance FromField QuestionnaireVisibility where
  fromField = fromFieldGenericEnum
