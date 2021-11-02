module Wizard.Database.Mapping.Questionnaire.Questionnaire where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclJM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionJM ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireSharing ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireVisibility ()
import Wizard.Model.Questionnaire.Questionnaire

instance ToRow Questionnaire where
  toRow Questionnaire {..} =
    [ toField _questionnaireUuid
    , toField _questionnaireName
    , toField _questionnaireVisibility
    , toField _questionnaireSharing
    , toField _questionnairePackageId
    , toJSONField _questionnaireSelectedTagUuids
    , toField _questionnaireTemplateId
    , toField _questionnaireFormatUuid
    , toField _questionnaireCreatorUuid
    , toJSONField _questionnaireEvents
    , toJSONField _questionnaireVersions
    , toField _questionnaireCreatedAt
    , toField _questionnaireUpdatedAt
    , toField _questionnaireDescription
    , toField _questionnaireIsTemplate
    , toField _questionnaireSquashed
    , toField _questionnaireAppUuid
    ]

instance FromRow Questionnaire where
  fromRow = do
    _questionnaireUuid <- field
    _questionnaireName <- field
    _questionnaireVisibility <- field
    _questionnaireSharing <- field
    _questionnairePackageId <- field
    _questionnaireSelectedTagUuids <- fieldWith fromJSONField
    _questionnaireTemplateId <- field
    _questionnaireFormatUuid <- field
    _questionnaireCreatorUuid <- field
    let _questionnairePermissions = []
    _questionnaireEvents <- fieldWith fromJSONField
    _questionnaireVersions <- fieldWith fromJSONField
    _questionnaireCreatedAt <- field
    _questionnaireUpdatedAt <- field
    _questionnaireDescription <- field
    _questionnaireIsTemplate <- field
    _questionnaireSquashed <- field
    _questionnaireAppUuid <- field
    return $ Questionnaire {..}
