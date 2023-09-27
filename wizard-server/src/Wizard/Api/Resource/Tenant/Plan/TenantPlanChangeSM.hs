module Wizard.Api.Resource.Tenant.Plan.TenantPlanChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Tenant.Plan.TenantPlanChangeDTO
import Wizard.Api.Resource.Tenant.Plan.TenantPlanChangeJM ()
import Wizard.Database.Migration.Development.Tenant.Data.TenantPlans
import Wizard.Service.Tenant.Plan.PlanMapper

instance ToSchema TenantPlanChangeDTO where
  declareNamedSchema = toSwagger (toChangeDTO standardPlan)
