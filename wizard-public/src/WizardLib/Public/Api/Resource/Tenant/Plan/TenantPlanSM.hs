module WizardLib.Public.Api.Resource.Tenant.Plan.TenantPlanSM where

import Data.Swagger

import Shared.Common.Api.Resource.Common.AesonSM ()
import Shared.Common.Util.Swagger
import WizardLib.Public.Api.Resource.Tenant.Plan.TenantPlanJM ()
import WizardLib.Public.Database.Migration.Development.Tenant.Data.TenantPlans
import WizardLib.Public.Model.Tenant.Plan.TenantPlan

instance ToSchema TenantPlan where
  declareNamedSchema = toSwagger standardPlan
