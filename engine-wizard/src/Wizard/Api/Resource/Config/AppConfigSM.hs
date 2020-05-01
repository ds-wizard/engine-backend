module Wizard.Api.Resource.Config.AppConfigSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Config.AppConfigJM ()
import Wizard.Api.Resource.Config.SimpleFeatureSM ()
import Wizard.Database.Migration.Development.Config.Data.AppConfigs
import Wizard.Model.Config.AppConfig

instance ToSchema AppConfig where
  declareNamedSchema = simpleToSchema' "_appConfig" defaultAppConfig

instance ToSchema AppConfigOrganization where
  declareNamedSchema = simpleToSchema' "_appConfigOrganization" defaultOrganization

instance ToSchema AppConfigAuth where
  declareNamedSchema = simpleToSchema' "_appConfigAuth" defaultAuth

instance ToSchema AppConfigAuthInternal where
  declareNamedSchema = simpleToSchema' "_appConfigAuthInternal" defaultAuthInternal

instance ToSchema AppConfigAuthExternal where
  declareNamedSchema = simpleToSchema' "_appConfigAuthExternal" defaultAuthExternal

instance ToSchema AppConfigAuthExternalService where
  declareNamedSchema = simpleToSchema' "_appConfigAuthExternalService" defaultAuthExternalService

instance ToSchema AppConfigAuthExternalServiceParameter where
  declareNamedSchema = simpleToSchema' "_appConfigAuthExternalServiceParameter" defaultAuthExternalServiceParameter

instance ToSchema AppConfigAuthExternalServiceStyle where
  declareNamedSchema = simpleToSchema' "_appConfigAuthExternalServiceStyle" defaultAuthExternalServiceStyle

instance ToSchema AppConfigPrivacyAndSupport where
  declareNamedSchema = simpleToSchema' "_appConfigPrivacyAndSupport" defaultPrivacyAndSupport

instance ToSchema AppConfigDashboard where
  declareNamedSchema = simpleToSchema' "_appConfigDashboard" defaultDashboard

instance ToSchema AppConfigDashboardWidgets where
  declareNamedSchema = simpleToSchema' "_appConfigDashboardWidgets" defaultDashboardWidgets

instance ToSchema AppConfigLookAndFeel where
  declareNamedSchema = simpleToSchema' "_appConfigLookAndFeel" defaultLookAndFeel

instance ToSchema AppConfigLookAndFeelCustomMenuLink where
  declareNamedSchema = simpleToSchema' "_appConfigLookAndFeelCustomMenuLink" defaultLookAndFeelCustomLink

instance ToSchema AppConfigRegistry where
  declareNamedSchema = simpleToSchema' "_appConfigRegistry" defaultRegistry

instance ToSchema AppConfigQuestionnaire where
  declareNamedSchema = simpleToSchema' "_appConfigQuestionnaire" defaultQuestionnaire

instance ToSchema AppConfigQuestionnaireFeedback where
  declareNamedSchema = simpleToSchema' "_appConfigQuestionnaireFeedback" defaultFeedback

instance ToSchema AppConfigSubmission where
  declareNamedSchema = simpleToSchema' "_appConfigSubmission" defaultSubmission

instance ToSchema AppConfigSubmissionService where
  declareNamedSchema = simpleToSchema' "_appConfigSubmissionService" defaultSubmissionService

instance ToSchema AppConfigSubmissionServiceSupportedFormat where
  declareNamedSchema =
    simpleToSchema' "_appConfigSubmissionServiceSupportedFormat" defaultSubmissionServiceSupportedFormat

instance ToSchema AppConfigSubmissionServiceRequest where
  declareNamedSchema = simpleToSchema' "_appConfigSubmissionServiceRequest" defaultSubmissionServiceRequest

instance ToSchema AppConfigSubmissionServiceRequestMultipart where
  declareNamedSchema =
    simpleToSchema' "_appConfigSubmissionServiceRequestMultipart" defaultSubmissionServiceRequestMultipart
