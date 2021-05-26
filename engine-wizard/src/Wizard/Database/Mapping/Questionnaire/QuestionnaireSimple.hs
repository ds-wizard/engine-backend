module Wizard.Database.Mapping.Questionnaire.QuestionnaireSimple where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Model.Questionnaire.QuestionnaireSimple

instance ToRow QuestionnaireSimple where
  toRow QuestionnaireSimple {..} = [toField _questionnaireSimpleUuid, toField _questionnaireSimpleName]

instance FromRow QuestionnaireSimple where
  fromRow = do
    _questionnaireSimpleUuid <- field
    _questionnaireSimpleName <- field
    return $ QuestionnaireSimple {..}
