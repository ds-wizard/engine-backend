module Wizard.Api.Resource.Tenant.TenantDetailSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Tenant.TenantDetailDTO
import Wizard.Api.Resource.Tenant.TenantDetailJM ()
import Wizard.Api.Resource.Tenant.Usage.TenantUsageSM ()
import Wizard.Api.Resource.User.UserSM ()
import Wizard.Database.Migration.Development.Tenant.Data.TenantUsages
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Service.Tenant.TenantMapper
import WizardLib.Public.Api.Resource.Tenant.Plan.TenantPlanSM ()
import WizardLib.Public.Database.Migration.Development.Tenant.Data.TenantPlans

instance ToSchema TenantDetailDTO where
  declareNamedSchema =
    toSwagger (toDetailDTO defaultTenant Nothing Nothing [standardPlan, standardPlanExpired] defaultTenantUsage [])
