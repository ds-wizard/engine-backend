module Wizard.Api.Resource.Tenant.Config.TenantConfigSM where

import Data.Swagger

import Shared.Common.Api.Resource.Config.SimpleFeatureSM ()
import Shared.Common.Util.Swagger
import Shared.OpenId.Api.Resource.OpenId.Client.Definition.OpenIdClientParameterSM ()
import Shared.OpenId.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilitySM ()
import Wizard.Api.Resource.Tenant.Config.TenantConfigJM ()
import Wizard.Database.Migration.Development.Tenant.Data.TenantConfigs
import Wizard.Model.Tenant.Config.TenantConfig
import WizardLib.KnowledgeModel.Api.Resource.Package.PackagePatternSM ()
import WizardLib.Public.Api.Resource.Tenant.Config.TenantConfigSM ()

instance ToSchema TenantConfig where
  declareNamedSchema = toSwagger defaultTenantConfig

instance ToSchema TenantConfigOrganization where
  declareNamedSchema = toSwagger defaultOrganization

instance ToSchema TenantConfigAuth where
  declareNamedSchema = toSwagger defaultAuth

instance ToSchema TenantConfigAuthInternal where
  declareNamedSchema = toSwagger defaultAuthInternal

instance ToSchema TenantConfigAuthInternalTwoFactorAuth where
  declareNamedSchema = toSwagger defaultAuthInternalTwoFactorAuth

instance ToSchema TenantConfigAuthExternal where
  declareNamedSchema = toSwagger defaultAuthExternal

instance ToSchema TenantConfigAuthExternalService where
  declareNamedSchema = toSwagger defaultAuthExternalService

instance ToSchema TenantConfigPrivacyAndSupport where
  declareNamedSchema = toSwagger defaultPrivacyAndSupport

instance ToSchema TenantConfigDashboardAndLoginScreen where
  declareNamedSchema = toSwagger defaultDashboardAndLoginScreen

instance ToSchema TenantConfigDashboardAndLoginScreenDashboardType

instance ToSchema TenantConfigLookAndFeel where
  declareNamedSchema = toSwagger defaultLookAndFeel

instance ToSchema TenantConfigLookAndFeelCustomMenuLink where
  declareNamedSchema = toSwagger defaultLookAndFeelCustomLink

instance ToSchema TenantConfigRegistry where
  declareNamedSchema = toSwagger defaultRegistry

instance ToSchema TenantConfigKnowledgeModel where
  declareNamedSchema = toSwagger defaultKnowledgeModel

instance ToSchema TenantConfigKnowledgeModelPublic where
  declareNamedSchema = toSwagger defaultKnowledgeModelPublic

instance ToSchema TenantConfigQuestionnaire where
  declareNamedSchema = toSwagger defaultQuestionnaire

instance ToSchema TenantConfigQuestionnaireVisibility where
  declareNamedSchema = toSwagger defaultQuestionnaireVisibility

instance ToSchema TenantConfigQuestionnaireSharing where
  declareNamedSchema = toSwagger defaultQuestionnaireSharing

instance ToSchema QuestionnaireCreation

instance ToSchema TenantConfigQuestionnaireProjectTagging where
  declareNamedSchema = toSwagger defaultQuestionnaireProjectTagging

instance ToSchema TenantConfigQuestionnaireFeedback where
  declareNamedSchema = toSwagger defaultFeedback

instance ToSchema TenantConfigSubmission where
  declareNamedSchema = toSwagger defaultSubmission

instance ToSchema TenantConfigSubmissionService where
  declareNamedSchema = toSwagger defaultSubmissionService

instance ToSchema TenantConfigSubmissionServiceSupportedFormat where
  declareNamedSchema =
    toSwagger defaultSubmissionServiceSupportedFormat

instance ToSchema TenantConfigSubmissionServiceRequest where
  declareNamedSchema = toSwagger defaultSubmissionServiceRequest

instance ToSchema TenantConfigSubmissionServiceRequestMultipart where
  declareNamedSchema =
    toSwagger defaultSubmissionServiceRequestMultipart

instance ToSchema TenantConfigOwl where
  declareNamedSchema = toSwagger defaultOwl
