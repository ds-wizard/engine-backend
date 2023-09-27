module Wizard.Api.Resource.Tenant.TenantCreateSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Tenant.TenantCreateDTO
import Wizard.Api.Resource.Tenant.TenantCreateJM ()
import Wizard.Database.Migration.Development.Tenant.Data.Tenants

instance ToSchema TenantCreateDTO where
  declareNamedSchema = toSwagger tenantCreateDto
