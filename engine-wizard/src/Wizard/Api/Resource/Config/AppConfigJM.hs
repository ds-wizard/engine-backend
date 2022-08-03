module Wizard.Api.Resource.Config.AppConfigJM where

import Data.Aeson

import Shared.Api.Resource.Package.PackagePatternJM ()
import Shared.Util.JSON
import Wizard.Api.Resource.Config.SimpleFeatureJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilityJM ()
import Wizard.Model.Config.AppConfig

instance FromJSON AppConfig where
  parseJSON = simpleParseJSON "_appConfig"

instance ToJSON AppConfig where
  toJSON = simpleToJSON "_appConfig"

instance FromJSON AppConfigOrganization where
  parseJSON = simpleParseJSON "_appConfigOrganization"

instance ToJSON AppConfigOrganization where
  toJSON = simpleToJSON "_appConfigOrganization"

instance FromJSON AppConfigFeature where
  parseJSON = simpleParseJSON "_appConfigFeature"

instance ToJSON AppConfigFeature where
  toJSON = simpleToJSON "_appConfigFeature"

instance FromJSON AppConfigAuth where
  parseJSON = simpleParseJSON "_appConfigAuth"

instance ToJSON AppConfigAuth where
  toJSON = simpleToJSON "_appConfigAuth"

instance FromJSON AppConfigAuthInternal where
  parseJSON = simpleParseJSON "_appConfigAuthInternal"

instance ToJSON AppConfigAuthInternal where
  toJSON = simpleToJSON "_appConfigAuthInternal"

instance FromJSON AppConfigAuthExternal where
  parseJSON = simpleParseJSON "_appConfigAuthExternal"

instance ToJSON AppConfigAuthExternal where
  toJSON = simpleToJSON "_appConfigAuthExternal"

instance FromJSON AppConfigAuthExternalService where
  parseJSON = simpleParseJSON "_appConfigAuthExternalService"

instance ToJSON AppConfigAuthExternalService where
  toJSON = simpleToJSON "_appConfigAuthExternalService"

instance FromJSON AppConfigAuthExternalServiceParameter where
  parseJSON = simpleParseJSON "_appConfigAuthExternalServiceParameter"

instance ToJSON AppConfigAuthExternalServiceParameter where
  toJSON = simpleToJSON "_appConfigAuthExternalServiceParameter"

instance FromJSON AppConfigAuthExternalServiceStyle where
  parseJSON = simpleParseJSON "_appConfigAuthExternalServiceStyle"

instance ToJSON AppConfigAuthExternalServiceStyle where
  toJSON = simpleToJSON "_appConfigAuthExternalServiceStyle"

instance FromJSON AppConfigPrivacyAndSupport where
  parseJSON = simpleParseJSON "_appConfigPrivacyAndSupport"

instance ToJSON AppConfigPrivacyAndSupport where
  toJSON = simpleToJSON "_appConfigPrivacyAndSupport"

instance FromJSON AppConfigDashboard where
  parseJSON = simpleParseJSON "_appConfigDashboard"

instance ToJSON AppConfigDashboard where
  toJSON = simpleToJSON "_appConfigDashboard"

instance FromJSON AppConfigDashboardDashboardType

instance ToJSON AppConfigDashboardDashboardType

instance FromJSON AppConfigLookAndFeel where
  parseJSON = simpleParseJSON "_appConfigLookAndFeel"

instance ToJSON AppConfigLookAndFeel where
  toJSON = simpleToJSON "_appConfigLookAndFeel"

instance FromJSON AppConfigLookAndFeelCustomMenuLink where
  parseJSON = simpleParseJSON "_appConfigLookAndFeelCustomMenuLink"

instance ToJSON AppConfigLookAndFeelCustomMenuLink where
  toJSON = simpleToJSON "_appConfigLookAndFeelCustomMenuLink"

instance FromJSON AppConfigRegistry where
  parseJSON = simpleParseJSON "_appConfigRegistry"

instance ToJSON AppConfigRegistry where
  toJSON = simpleToJSON "_appConfigRegistry"

instance FromJSON AppConfigKnowledgeModel where
  parseJSON = simpleParseJSON "_appConfigKnowledgeModel"

instance ToJSON AppConfigKnowledgeModel where
  toJSON = simpleToJSON "_appConfigKnowledgeModel"

instance FromJSON AppConfigKnowledgeModelPublic where
  parseJSON = simpleParseJSON "_appConfigKnowledgeModelPublic"

instance ToJSON AppConfigKnowledgeModelPublic where
  toJSON = simpleToJSON "_appConfigKnowledgeModelPublic"

instance FromJSON AppConfigQuestionnaire where
  parseJSON = simpleParseJSON "_appConfigQuestionnaire"

instance ToJSON AppConfigQuestionnaire where
  toJSON = simpleToJSON "_appConfigQuestionnaire"

instance FromJSON AppConfigQuestionnaireVisibility where
  parseJSON = simpleParseJSON "_appConfigQuestionnaireVisibility"

instance ToJSON AppConfigQuestionnaireVisibility where
  toJSON = simpleToJSON "_appConfigQuestionnaireVisibility"

instance FromJSON AppConfigQuestionnaireSharing where
  parseJSON = simpleParseJSON "_appConfigQuestionnaireSharing"

instance ToJSON AppConfigQuestionnaireSharing where
  toJSON = simpleToJSON "_appConfigQuestionnaireSharing"

instance FromJSON QuestionnaireCreation

instance ToJSON QuestionnaireCreation

instance FromJSON AppConfigQuestionnaireProjectTagging where
  parseJSON = simpleParseJSON "_appConfigQuestionnaireProjectTagging"

instance ToJSON AppConfigQuestionnaireProjectTagging where
  toJSON = simpleToJSON "_appConfigQuestionnaireProjectTagging"

instance FromJSON AppConfigQuestionnaireFeedback where
  parseJSON = simpleParseJSON "_appConfigQuestionnaireFeedback"

instance ToJSON AppConfigQuestionnaireFeedback where
  toJSON = simpleToJSON "_appConfigQuestionnaireFeedback"

instance FromJSON AppConfigTemplate where
  parseJSON = simpleParseJSON "_appConfigTemplate"

instance ToJSON AppConfigTemplate where
  toJSON = simpleToJSON "_appConfigTemplate"

instance FromJSON AppConfigSubmission where
  parseJSON = simpleParseJSON "_appConfigSubmission"

instance ToJSON AppConfigSubmission where
  toJSON = simpleToJSON "_appConfigSubmission"

instance FromJSON AppConfigSubmissionService where
  parseJSON = simpleParseJSON "_appConfigSubmissionService"

instance ToJSON AppConfigSubmissionService where
  toJSON = simpleToJSON "_appConfigSubmissionService"

instance FromJSON AppConfigSubmissionServiceSupportedFormat where
  parseJSON = simpleParseJSON "_appConfigSubmissionServiceSupportedFormat"

instance ToJSON AppConfigSubmissionServiceSupportedFormat where
  toJSON = simpleToJSON "_appConfigSubmissionServiceSupportedFormat"

instance FromJSON AppConfigSubmissionServiceRequest where
  parseJSON = simpleParseJSON "_appConfigSubmissionServiceRequest"

instance ToJSON AppConfigSubmissionServiceRequest where
  toJSON = simpleToJSON "_appConfigSubmissionServiceRequest"

instance FromJSON AppConfigSubmissionServiceRequestMultipart where
  parseJSON = simpleParseJSON "_appConfigSubmissionServiceRequestMultipart"

instance ToJSON AppConfigSubmissionServiceRequestMultipart where
  toJSON = simpleToJSON "_appConfigSubmissionServiceRequestMultipart"

instance FromJSON AppConfigOwl where
  parseJSON = simpleParseJSON "_appConfigOwl"

instance ToJSON AppConfigOwl where
  toJSON = simpleToJSON "_appConfigOwl"
