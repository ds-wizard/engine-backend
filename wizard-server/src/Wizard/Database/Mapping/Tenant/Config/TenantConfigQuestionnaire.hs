module Wizard.Database.Mapping.Tenant.Config.TenantConfigQuestionnaire where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Shared.Common.Model.Config.SimpleFeature
import Wizard.Database.Mapping.Questionnaire.QuestionnaireCreation ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireSharing ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireVisibility ()
import Wizard.Model.Tenant.Config.TenantConfig

instance ToRow TenantConfigQuestionnaire where
  toRow TenantConfigQuestionnaire {..} =
    [ toField tenantUuid
    , toField questionnaireVisibility.enabled
    , toField questionnaireVisibility.defaultValue
    , toField questionnaireSharing.enabled
    , toField questionnaireSharing.defaultValue
    , toField questionnaireSharing.anonymousEnabled
    , toField questionnaireCreation
    , toField projectTagging.enabled
    , toField . PGArray $ projectTagging.tags
    , toField summaryReport.enabled
    , toField feedback.enabled
    , toField feedback.token
    , toField feedback.owner
    , toField feedback.repo
    , toField createdAt
    , toField updatedAt
    ]

instance FromRow TenantConfigQuestionnaire where
  fromRow = do
    tenantUuid <- field
    questionnaireVisibilityEnabled <- field
    questionnaireVisibilityDefaultValue <- field
    let questionnaireVisibility =
          TenantConfigQuestionnaireVisibility
            { enabled = questionnaireVisibilityEnabled
            , defaultValue = questionnaireVisibilityDefaultValue
            }
    questionnaireSharingEnabled <- field
    questionnaireSharingDefaultValue <- field
    questionnaireSharingAnonymousEnabled <- field
    let questionnaireSharing =
          TenantConfigQuestionnaireSharing
            { enabled = questionnaireSharingEnabled
            , defaultValue = questionnaireSharingDefaultValue
            , anonymousEnabled = questionnaireSharingAnonymousEnabled
            }
    questionnaireCreation <- field
    projectTaggingEnabled <- field
    projectTaggingTags <- fromPGArray <$> field
    let projectTagging =
          TenantConfigQuestionnaireProjectTagging
            { enabled = projectTaggingEnabled
            , tags = projectTaggingTags
            }
    summaryReportEnabled <- field
    let summaryReport = SimpleFeature summaryReportEnabled
    feedbackEnabled <- field
    feedbackToken <- field
    feedbackOwner <- field
    feedbackRepo <- field
    let feedback =
          TenantConfigQuestionnaireFeedback
            { enabled = feedbackEnabled
            , token = feedbackToken
            , owner = feedbackOwner
            , repo = feedbackRepo
            }
    createdAt <- field
    updatedAt <- field
    return TenantConfigQuestionnaire {..}
