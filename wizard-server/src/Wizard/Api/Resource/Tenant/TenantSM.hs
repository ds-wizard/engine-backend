module Wizard.Api.Resource.Tenant.TenantSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Tenant.TenantDTO
import Wizard.Api.Resource.Tenant.TenantJM ()
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Model.Tenant.Tenant
import Wizard.Service.Tenant.TenantMapper

instance ToSchema Tenant where
  declareNamedSchema = toSwagger defaultTenant

instance ToSchema TenantState where
  declareNamedSchema = toSwagger ReadyForUseTenantState

instance ToParamSchema TenantState

instance ToSchema TenantDTO where
  declareNamedSchema = toSwagger (toDTO defaultTenant Nothing Nothing)
