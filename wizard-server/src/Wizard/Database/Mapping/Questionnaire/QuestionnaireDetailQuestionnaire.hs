module Wizard.Database.Mapping.Questionnaire.QuestionnaireDetailQuestionnaire where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow

import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventJM ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireAcl
import Wizard.Database.Mapping.Questionnaire.QuestionnaireSharing ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireState ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireVisibility ()
import Wizard.Model.Questionnaire.QuestionnaireDetailQuestionnaire

instance FromRow QuestionnaireDetailQuestionnaire where
  fromRow = do
    uuid <- field
    name <- field
    visibility <- field
    sharing <- field
    packageId <- field
    selectedQuestionTagUuids <- fieldWith fromJSONField
    isTemplate <- field
    events <- fieldWith fromJSONField
    migrationUuid <- field
    permissions <- loadPermissions uuid
    questionnaireActionsAvailable <- field
    questionnaireImportersAvailable <- field
    return $ QuestionnaireDetailQuestionnaire {..}
