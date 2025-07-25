module Wizard.Api.Resource.Tenant.Config.TenantConfigChangeSM where

import Data.Swagger

import Shared.Common.Api.Resource.Config.SimpleFeatureSM ()
import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Tenant.Config.TenantConfigChangeDTO
import Wizard.Api.Resource.Tenant.Config.TenantConfigChangeJM ()
import Wizard.Api.Resource.Tenant.Config.TenantConfigSM ()
import Wizard.Database.Migration.Development.Tenant.Data.TenantConfigs
import WizardLib.Public.Api.Resource.Tenant.Config.TenantConfigChangeSM ()

instance ToSchema TenantConfigChangeDTO where
  declareNamedSchema = toSwagger defaultTenantConfigChangeDto

instance ToSchema TenantConfigOrganizationChangeDTO where
  declareNamedSchema = toSwagger defaultOrganizationChangeDto

instance ToSchema TenantConfigAuthenticationChangeDTO where
  declareNamedSchema = toSwagger defaultAuthenticationChangeDto

instance ToSchema TenantConfigAuthenticationExternalChangeDTO where
  declareNamedSchema = toSwagger defaultAuthenticationExternalChangeDto

instance ToSchema TenantConfigAuthenticationExternalServiceChangeDTO where
  declareNamedSchema = toSwagger defaultAuthenticationExternalServiceChangeDto

instance ToSchema TenantConfigPrivacyAndSupportChangeDTO where
  declareNamedSchema = toSwagger defaultPrivacyAndSupportChangeDto

instance ToSchema TenantConfigDashboardAndLoginScreenChangeDTO where
  declareNamedSchema = toSwagger defaultDashboardAndLoginScreenChangeDto

instance ToSchema TenantConfigRegistryChangeDTO where
  declareNamedSchema = toSwagger defaultRegistryChangeDto

instance ToSchema TenantConfigKnowledgeModelChangeDTO where
  declareNamedSchema = toSwagger defaultKnowledgeModelChangeDto

instance ToSchema TenantConfigKnowledgeModelPublicChangeDTO where
  declareNamedSchema = toSwagger defaultKnowledgeModelPublicChangeDto

instance ToSchema TenantConfigQuestionnaireChangeDTO where
  declareNamedSchema = toSwagger defaultQuestionnaireChangeDto

instance ToSchema TenantConfigSubmissionChangeDTO where
  declareNamedSchema = toSwagger defaultSubmission

instance ToSchema TenantConfigSubmissionServiceChangeDTO where
  declareNamedSchema = toSwagger defaultSubmissionService

instance ToSchema TenantConfigSubmissionServiceSupportedFormatChangeDTO where
  declareNamedSchema = toSwagger defaultSubmissionServiceSupportedFormat

instance ToSchema TenantConfigSubmissionServiceRequestChangeDTO where
  declareNamedSchema = toSwagger defaultSubmissionServiceRequest

instance ToSchema TenantConfigSubmissionServiceRequestMultipartChangeDTO where
  declareNamedSchema = toSwagger defaultSubmissionServiceRequestMultipart

instance ToSchema TenantConfigFeaturesChangeDTO where
  declareNamedSchema = toSwagger defaultSubmissionServiceRequestMultipart
