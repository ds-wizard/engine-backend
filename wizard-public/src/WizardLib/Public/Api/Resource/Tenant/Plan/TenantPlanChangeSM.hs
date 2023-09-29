module WizardLib.Public.Api.Resource.Tenant.Plan.TenantPlanChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.Public.Api.Resource.Tenant.Plan.TenantPlanChangeDTO
import WizardLib.Public.Api.Resource.Tenant.Plan.TenantPlanChangeJM ()
import WizardLib.Public.Database.Migration.Development.Tenant.Data.TenantPlans
import WizardLib.Public.Service.Tenant.Plan.PlanMapper

instance ToSchema TenantPlanChangeDTO where
  declareNamedSchema = toSwagger (toChangeDTO standardPlan)
