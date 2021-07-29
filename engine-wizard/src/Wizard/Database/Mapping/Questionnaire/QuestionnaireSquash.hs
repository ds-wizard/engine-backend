module Wizard.Database.Mapping.Questionnaire.QuestionnaireSquash where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventJM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionJM ()
import Wizard.Model.Questionnaire.QuestionnaireSquash

instance ToRow QuestionnaireSquash where
  toRow QuestionnaireSquash {..} =
    [toField _questionnaireSquashUuid, toJSONField _questionnaireSquashEvents, toJSONField _questionnaireSquashVersions]

instance FromRow QuestionnaireSquash where
  fromRow = do
    _questionnaireSquashUuid <- field
    _questionnaireSquashEvents <- fieldWith fromJSONField
    _questionnaireSquashVersions <- fieldWith fromJSONField
    return $ QuestionnaireSquash {..}
