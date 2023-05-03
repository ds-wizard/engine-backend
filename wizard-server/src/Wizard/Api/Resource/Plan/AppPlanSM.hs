module Wizard.Api.Resource.Plan.AppPlanSM where

import Data.Swagger

import Shared.Common.Api.Resource.Common.AesonSM ()
import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Plan.AppPlanJM ()
import Wizard.Database.Migration.Development.Plan.Data.AppPlans
import Wizard.Model.Plan.AppPlan

instance ToSchema AppPlan where
  declareNamedSchema = toSwagger standardPlan
