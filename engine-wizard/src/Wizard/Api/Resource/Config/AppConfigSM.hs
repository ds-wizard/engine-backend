module Wizard.Api.Resource.Config.AppConfigSM where

import Data.Swagger

import Shared.Api.Resource.Package.PackagePatternSM ()
import Shared.Util.Swagger
import Wizard.Api.Resource.Config.AppConfigJM ()
import Wizard.Api.Resource.Config.SimpleFeatureSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilitySM ()
import Wizard.Database.Migration.Development.Config.Data.AppConfigs
import Wizard.Model.Config.AppConfig

instance ToSchema AppConfig where
  declareNamedSchema = simpleToSchema' "_appConfig" defaultAppConfig

instance ToSchema AppConfigOrganization where
  declareNamedSchema = simpleToSchema' "_appConfigOrganization" defaultOrganization

instance ToSchema AppConfigFeature where
  declareNamedSchema = simpleToSchema' "_appConfigFeature" defaultFeature

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

instance ToSchema AppConfigKnowledgeModel where
  declareNamedSchema = simpleToSchema' "_appConfigKnowledgeModel" defaultKnowledgeModel

instance ToSchema AppConfigKnowledgeModelPublic where
  declareNamedSchema = simpleToSchema' "_appConfigKnowledgeModelPublic" defaultKnowledgeModelPublic

instance ToSchema AppConfigQuestionnaire where
  declareNamedSchema = simpleToSchema' "_appConfigQuestionnaire" defaultQuestionnaire

instance ToSchema AppConfigQuestionnaireVisibility where
  declareNamedSchema = simpleToSchema' "_appConfigQuestionnaireVisibility" defaultQuestionnaireVisibility

instance ToSchema AppConfigQuestionnaireSharing where
  declareNamedSchema = simpleToSchema' "_appConfigQuestionnaireSharing" defaultQuestionnaireSharing

instance ToSchema QuestionnaireCreation

instance ToSchema AppConfigQuestionnaireFeedback where
  declareNamedSchema = simpleToSchema' "_appConfigQuestionnaireFeedback" defaultFeedback

instance ToSchema AppConfigTemplate where
  declareNamedSchema = simpleToSchema' "_appConfigTemplate" defaultTemplate

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
