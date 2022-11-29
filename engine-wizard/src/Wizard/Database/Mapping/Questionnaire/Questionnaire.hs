module Wizard.Database.Mapping.Questionnaire.Questionnaire where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclJM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionJM ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireSharing ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireVisibility ()
import Wizard.Model.Questionnaire.Questionnaire

instance ToRow Questionnaire where
  toRow Questionnaire {..} =
    [ toField uuid
    , toField name
    , toField visibility
    , toField sharing
    , toField packageId
    , toJSONField selectedQuestionTagUuids
    , toField templateId
    , toField formatUuid
    , toField creatorUuid
    , toJSONField events
    , toJSONField versions
    , toField createdAt
    , toField updatedAt
    , toField description
    , toField isTemplate
    , toField squashed
    , toField appUuid
    , toField . PGArray $ projectTags
    , toField answeredQuestions
    , toField unansweredQuestions
    ]

instance FromRow Questionnaire where
  fromRow = do
    uuid <- field
    name <- field
    visibility <- field
    sharing <- field
    packageId <- field
    selectedQuestionTagUuids <- fieldWith fromJSONField
    templateId <- field
    formatUuid <- field
    creatorUuid <- field
    let permissions = []
    events <- fieldWith fromJSONField
    versions <- fieldWith fromJSONField
    createdAt <- field
    updatedAt <- field
    description <- field
    isTemplate <- field
    squashed <- field
    appUuid <- field
    projectTags <- fromPGArray <$> field
    answeredQuestions <- field
    unansweredQuestions <- field
    return $ Questionnaire {..}
