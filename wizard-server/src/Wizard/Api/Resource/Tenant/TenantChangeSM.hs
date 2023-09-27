module Wizard.Api.Resource.Tenant.TenantChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Tenant.TenantChangeDTO
import Wizard.Api.Resource.Tenant.TenantChangeJM ()
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Service.Tenant.TenantMapper

instance ToSchema TenantChangeDTO where
  declareNamedSchema = toSwagger (toChangeDTO defaultTenant)
