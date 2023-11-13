module Wizard.Api.Resource.Tenant.Usage.TenantUsageSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Tenant.Usage.TenantUsageDTO
import Wizard.Api.Resource.Tenant.Usage.TenantUsageJM ()
import Wizard.Database.Migration.Development.Tenant.Data.TenantUsages

instance ToSchema TenantUsageDTO where
  declareNamedSchema = toSwagger defaultTenantUsage

instance ToSchema TenantUsageEntryDTO where
  declareNamedSchema = toSwagger defaultTenantUsageUsers
