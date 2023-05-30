module Wizard.Api.Resource.Config.AppConfigJM where

import Data.Aeson

import Shared.Common.Api.Resource.Config.SimpleFeatureJM ()
import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilityJM ()
import Wizard.Model.Config.AppConfig
import WizardLib.KnowledgeModel.Api.Resource.Package.PackagePatternJM ()

instance FromJSON AppConfig where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfig where
  toJSON = genericToJSON jsonOptions

instance FromJSON AppConfigOrganization where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigOrganization where
  toJSON = genericToJSON jsonOptions

instance FromJSON AppConfigFeature where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigFeature where
  toJSON = genericToJSON jsonOptions

instance FromJSON AppConfigAuth where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigAuth where
  toJSON = genericToJSON jsonOptions

instance FromJSON AppConfigAuthInternal where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigAuthInternal where
  toJSON = genericToJSON jsonOptions

instance FromJSON AppConfigAuthInternalTwoFactorAuth where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigAuthInternalTwoFactorAuth where
  toJSON = genericToJSON jsonOptions

instance FromJSON AppConfigAuthExternal where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigAuthExternal where
  toJSON = genericToJSON jsonOptions

instance FromJSON AppConfigAuthExternalService where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigAuthExternalService where
  toJSON = genericToJSON jsonOptions

instance FromJSON AppConfigAuthExternalServiceParameter where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigAuthExternalServiceParameter where
  toJSON = genericToJSON jsonOptions

instance FromJSON AppConfigAuthExternalServiceStyle where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigAuthExternalServiceStyle where
  toJSON = genericToJSON jsonOptions

instance FromJSON AppConfigPrivacyAndSupport where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigPrivacyAndSupport where
  toJSON = genericToJSON jsonOptions

instance FromJSON AppConfigDashboardAndLoginScreen where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigDashboardAndLoginScreen where
  toJSON = genericToJSON jsonOptions

instance FromJSON AppConfigDashboardAndLoginScreenDashboardType

instance ToJSON AppConfigDashboardAndLoginScreenDashboardType

instance FromJSON AppConfigDashboardAndLoginScreenAnnouncement where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigDashboardAndLoginScreenAnnouncement where
  toJSON = genericToJSON jsonOptions

instance FromJSON AppConfigDashboardAndLoginScreenAnnouncementLevelType

instance ToJSON AppConfigDashboardAndLoginScreenAnnouncementLevelType

instance FromJSON AppConfigLookAndFeel where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigLookAndFeel where
  toJSON = genericToJSON jsonOptions

instance FromJSON AppConfigLookAndFeelCustomMenuLink where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigLookAndFeelCustomMenuLink where
  toJSON = genericToJSON jsonOptions

instance FromJSON AppConfigRegistry where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigRegistry where
  toJSON = genericToJSON jsonOptions

instance FromJSON AppConfigKnowledgeModel where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigKnowledgeModel where
  toJSON = genericToJSON jsonOptions

instance FromJSON AppConfigKnowledgeModelPublic where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigKnowledgeModelPublic where
  toJSON = genericToJSON jsonOptions

instance FromJSON AppConfigQuestionnaire where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigQuestionnaire where
  toJSON = genericToJSON jsonOptions

instance FromJSON AppConfigQuestionnaireVisibility where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigQuestionnaireVisibility where
  toJSON = genericToJSON jsonOptions

instance FromJSON AppConfigQuestionnaireSharing where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigQuestionnaireSharing where
  toJSON = genericToJSON jsonOptions

instance FromJSON QuestionnaireCreation

instance ToJSON QuestionnaireCreation

instance FromJSON AppConfigQuestionnaireProjectTagging where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigQuestionnaireProjectTagging where
  toJSON = genericToJSON jsonOptions

instance FromJSON AppConfigQuestionnaireFeedback where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigQuestionnaireFeedback where
  toJSON = genericToJSON jsonOptions

instance FromJSON AppConfigSubmission where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigSubmission where
  toJSON = genericToJSON jsonOptions

instance FromJSON AppConfigSubmissionService where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigSubmissionService where
  toJSON = genericToJSON jsonOptions

instance FromJSON AppConfigSubmissionServiceSupportedFormat where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigSubmissionServiceSupportedFormat where
  toJSON = genericToJSON jsonOptions

instance FromJSON AppConfigSubmissionServiceRequest where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigSubmissionServiceRequest where
  toJSON = genericToJSON jsonOptions

instance FromJSON AppConfigSubmissionServiceRequestMultipart where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigSubmissionServiceRequestMultipart where
  toJSON = genericToJSON jsonOptions

instance FromJSON AppConfigOwl where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigOwl where
  toJSON = genericToJSON jsonOptions
