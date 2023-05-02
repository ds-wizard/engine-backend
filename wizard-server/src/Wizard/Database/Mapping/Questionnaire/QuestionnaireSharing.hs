module Wizard.Database.Mapping.Questionnaire.QuestionnaireSharing where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Common.Database.Mapping.Common
import Wizard.Model.Questionnaire.Questionnaire

instance ToField QuestionnaireSharing where
  toField = toFieldGenericEnum

instance FromField QuestionnaireSharing where
  fromField = fromFieldGenericEnum
