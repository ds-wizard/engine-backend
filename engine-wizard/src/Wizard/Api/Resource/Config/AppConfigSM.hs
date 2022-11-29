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
  declareNamedSchema = toSwagger defaultAppConfig

instance ToSchema AppConfigOrganization where
  declareNamedSchema = toSwagger defaultOrganization

instance ToSchema AppConfigFeature where
  declareNamedSchema = toSwagger defaultFeature

instance ToSchema AppConfigAuth where
  declareNamedSchema = toSwagger defaultAuth

instance ToSchema AppConfigAuthInternal where
  declareNamedSchema = toSwagger defaultAuthInternal

instance ToSchema AppConfigAuthExternal where
  declareNamedSchema = toSwagger defaultAuthExternal

instance ToSchema AppConfigAuthExternalService where
  declareNamedSchema = toSwagger defaultAuthExternalService

instance ToSchema AppConfigAuthExternalServiceParameter where
  declareNamedSchema = toSwagger defaultAuthExternalServiceParameter

instance ToSchema AppConfigAuthExternalServiceStyle where
  declareNamedSchema = toSwagger defaultAuthExternalServiceStyle

instance ToSchema AppConfigPrivacyAndSupport where
  declareNamedSchema = toSwagger defaultPrivacyAndSupport

instance ToSchema AppConfigDashboard where
  declareNamedSchema = toSwagger defaultDashboard

instance ToSchema AppConfigDashboardDashboardType

instance ToSchema AppConfigLookAndFeel where
  declareNamedSchema = toSwagger defaultLookAndFeel

instance ToSchema AppConfigLookAndFeelCustomMenuLink where
  declareNamedSchema = toSwagger defaultLookAndFeelCustomLink

instance ToSchema AppConfigRegistry where
  declareNamedSchema = toSwagger defaultRegistry

instance ToSchema AppConfigKnowledgeModel where
  declareNamedSchema = toSwagger defaultKnowledgeModel

instance ToSchema AppConfigKnowledgeModelPublic where
  declareNamedSchema = toSwagger defaultKnowledgeModelPublic

instance ToSchema AppConfigQuestionnaire where
  declareNamedSchema = toSwagger defaultQuestionnaire

instance ToSchema AppConfigQuestionnaireVisibility where
  declareNamedSchema = toSwagger defaultQuestionnaireVisibility

instance ToSchema AppConfigQuestionnaireSharing where
  declareNamedSchema = toSwagger defaultQuestionnaireSharing

instance ToSchema QuestionnaireCreation

instance ToSchema AppConfigQuestionnaireProjectTagging where
  declareNamedSchema = toSwagger defaultQuestionnaireProjectTagging

instance ToSchema AppConfigQuestionnaireFeedback where
  declareNamedSchema = toSwagger defaultFeedback

instance ToSchema AppConfigTemplate where
  declareNamedSchema = toSwagger defaultTemplate

instance ToSchema AppConfigSubmission where
  declareNamedSchema = toSwagger defaultSubmission

instance ToSchema AppConfigSubmissionService where
  declareNamedSchema = toSwagger defaultSubmissionService

instance ToSchema AppConfigSubmissionServiceSupportedFormat where
  declareNamedSchema =
    toSwagger defaultSubmissionServiceSupportedFormat

instance ToSchema AppConfigSubmissionServiceRequest where
  declareNamedSchema = toSwagger defaultSubmissionServiceRequest

instance ToSchema AppConfigSubmissionServiceRequestMultipart where
  declareNamedSchema =
    toSwagger defaultSubmissionServiceRequestMultipart

instance ToSchema AppConfigOwl where
  declareNamedSchema = toSwagger defaultOwl
