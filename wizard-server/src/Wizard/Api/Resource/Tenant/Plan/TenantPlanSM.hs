module Wizard.Api.Resource.Tenant.Plan.TenantPlanSM where

import Data.Swagger

import Shared.Common.Api.Resource.Common.AesonSM ()
import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Tenant.Plan.TenantPlanJM ()
import Wizard.Database.Migration.Development.Tenant.Data.TenantPlans
import Wizard.Model.Tenant.Plan.TenantPlan

instance ToSchema TenantPlan where
  declareNamedSchema = toSwagger standardPlan
