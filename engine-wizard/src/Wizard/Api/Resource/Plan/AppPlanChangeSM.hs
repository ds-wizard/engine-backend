module Wizard.Api.Resource.Plan.AppPlanChangeSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Plan.AppPlanChangeDTO
import Wizard.Api.Resource.Plan.AppPlanChangeJM ()
import Wizard.Database.Migration.Development.Plan.Data.AppPlans
import Wizard.Service.Plan.AppPlanMapper

instance ToSchema AppPlanChangeDTO where
  declareNamedSchema = simpleToSchema (toChangeDTO standardPlan)
