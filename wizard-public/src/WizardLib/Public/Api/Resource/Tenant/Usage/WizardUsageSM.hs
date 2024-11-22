module WizardLib.Public.Api.Resource.Tenant.Usage.WizardUsageSM where

import Data.Swagger

import Shared.Common.Api.Resource.Tenant.Usage.UsageEntrySM ()
import Shared.Common.Util.Swagger
import WizardLib.Public.Api.Resource.Tenant.Usage.WizardUsageDTO
import WizardLib.Public.Api.Resource.Tenant.Usage.WizardUsageJM ()
import WizardLib.Public.Database.Migration.Development.Tenant.Data.TenantUsages

instance ToSchema WizardUsageDTO where
  declareNamedSchema = toSwagger defaultUsage
