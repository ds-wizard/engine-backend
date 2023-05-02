module Wizard.Database.Mapping.Questionnaire.QuestionnaireEvent where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow

import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventJM ()
import Wizard.Model.Questionnaire.QuestionnaireEvent

instance FromRow QuestionnaireEventBundle where
  fromRow = do
    events <- fieldWith fromJSONField
    return $ QuestionnaireEventBundle {..}
