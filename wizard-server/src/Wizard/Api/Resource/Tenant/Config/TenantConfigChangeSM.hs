module Wizard.Api.Resource.Tenant.Config.TenantConfigChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Tenant.Config.TenantConfigChangeDTO
import Wizard.Api.Resource.Tenant.Config.TenantConfigChangeJM ()
import Wizard.Api.Resource.Tenant.Config.TenantConfigSM ()
import Wizard.Database.Migration.Development.Tenant.Data.TenantConfigs
import Wizard.Service.Tenant.Config.ConfigMapper

instance ToSchema TenantConfigChangeDTO where
  declareNamedSchema = toSwagger (toChangeDTO defaultTenantConfig defaultSubmissionChangeDto)

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
