module Wizard.Api.Resource.Config.AppConfigJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Config.SimpleFeatureJM ()
import Wizard.Model.Config.AppConfig

instance FromJSON AppConfig where
  parseJSON = simpleParseJSON "_appConfig"

instance ToJSON AppConfig where
  toJSON = simpleToJSON "_appConfig"

instance FromJSON AppConfigOrganization where
  parseJSON = simpleParseJSON "_appConfigOrganization"

instance ToJSON AppConfigOrganization where
  toJSON = simpleToJSON "_appConfigOrganization"

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

instance FromJSON AppConfigDashboardWidgets where
  parseJSON = simpleParseJSON "_appConfigDashboardWidgets"

instance ToJSON AppConfigDashboardWidgets where
  toJSON = simpleToJSON "_appConfigDashboardWidgets"

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

instance FromJSON AppConfigQuestionnaire where
  parseJSON = simpleParseJSON "_appConfigQuestionnaire"

instance ToJSON AppConfigQuestionnaire where
  toJSON = simpleToJSON "_appConfigQuestionnaire"

instance FromJSON AppConfigQuestionnaireFeedback where
  parseJSON = simpleParseJSON "_appConfigQuestionnaireFeedback"

instance ToJSON AppConfigQuestionnaireFeedback where
  toJSON = simpleToJSON "_appConfigQuestionnaireFeedback"

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
