module Wizard.Database.Mapping.Questionnaire.QuestionnaireSquash where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionJM ()
import Wizard.Model.Questionnaire.QuestionnaireSquash

instance ToRow QuestionnaireSquash where
  toRow QuestionnaireSquash {..} =
    [toField uuid, toJSONField versions]

instance FromRow QuestionnaireSquash where
  fromRow = do
    uuid <- field
    versions <- fieldWith fromJSONField
    return $ QuestionnaireSquash {..}
