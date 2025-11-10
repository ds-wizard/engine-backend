module Wizard.Database.Mapping.Questionnaire.QuestionnaireDetail where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventJM ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireAcl
import Wizard.Database.Mapping.Questionnaire.QuestionnaireSharing ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireState ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireVisibility ()
import Wizard.Model.Questionnaire.QuestionnaireDetail

instance FromRow QuestionnaireDetail where
  fromRow = do
    uuid <- field
    name <- field
    visibility <- field
    sharing <- field
    knowledgeModelPackageId <- field
    selectedQuestionTagUuids <- fromPGArray <$> field
    isTemplate <- field
    migrationUuid <- field
    permissions <- loadPermissions uuid
    questionnaireActionsAvailable <- field
    questionnaireImportersAvailable <- field
    fileCount <- field
    return $ QuestionnaireDetail {..}
