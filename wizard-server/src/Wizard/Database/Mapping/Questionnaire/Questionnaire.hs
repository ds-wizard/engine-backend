module Wizard.Database.Mapping.Questionnaire.Questionnaire where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnairePermJM ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireSharing ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireVisibility ()
import Wizard.Model.Questionnaire.Questionnaire

instance ToRow Questionnaire where
  toRow Questionnaire {..} =
    [ toField uuid
    , toField name
    , toField visibility
    , toField sharing
    , toField knowledgeModelPackageId
    , toField . PGArray $ selectedQuestionTagUuids
    , toField documentTemplateId
    , toField formatUuid
    , toField creatorUuid
    , toField createdAt
    , toField updatedAt
    , toField description
    , toField isTemplate
    , toField squashed
    , toField tenantUuid
    , toField . PGArray $ projectTags
    ]

instance FromRow Questionnaire where
  fromRow = do
    uuid <- field
    name <- field
    visibility <- field
    sharing <- field
    knowledgeModelPackageId <- field
    selectedQuestionTagUuids <- fromPGArray <$> field
    documentTemplateId <- field
    formatUuid <- field
    creatorUuid <- field
    let permissions = []
    createdAt <- field
    updatedAt <- field
    description <- field
    isTemplate <- field
    squashed <- field
    tenantUuid <- field
    projectTags <- fromPGArray <$> field
    return $ Questionnaire {..}
