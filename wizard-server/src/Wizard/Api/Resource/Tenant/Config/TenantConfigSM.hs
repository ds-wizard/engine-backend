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

instance ToSchema TenantConfigAuthentication where
  declareNamedSchema = toSwagger defaultAuthentication

instance ToSchema TenantConfigAuthenticationInternal where
  declareNamedSchema = toSwagger defaultAuthenticationInternal

instance ToSchema TenantConfigAuthenticationInternalTwoFactorAuth where
  declareNamedSchema = toSwagger defaultAuthenticationInternalTwoFactorAuth

instance ToSchema TenantConfigAuthenticationExternal where
  declareNamedSchema = toSwagger defaultAuthenticationExternal

instance ToSchema TenantConfigAuthenticationExternalService where
  declareNamedSchema = toSwagger defaultAuthenticationExternalService

instance ToSchema TenantConfigPrivacyAndSupport where
  declareNamedSchema = toSwagger defaultPrivacyAndSupport

instance ToSchema TenantConfigDashboardAndLoginScreen where
  declareNamedSchema = toSwagger defaultDashboardAndLoginScreen

instance ToSchema TenantConfigDashboardAndLoginScreenDashboardType

instance ToSchema TenantConfigRegistry where
  declareNamedSchema = toSwagger defaultRegistry

instance ToSchema TenantConfigKnowledgeModel where
  declareNamedSchema = toSwagger defaultKnowledgeModel

instance ToSchema TenantConfigKnowledgeModelPublic where
  declareNamedSchema = toSwagger defaultKnowledgeModelPublic

instance ToSchema TenantConfigKnowledgeModelPublicPackagePattern where
  declareNamedSchema = toSwagger defaultKnowledgeModelPublicPackagePattern

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
