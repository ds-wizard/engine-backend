module Wizard.Api.Resource.App.AppDetailSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.App.AppDetailDTO
import Wizard.Api.Resource.App.AppDetailJM ()
import Wizard.Api.Resource.Plan.AppPlanSM ()
import Wizard.Api.Resource.Usage.UsageSM ()
import Wizard.Api.Resource.User.UserSM ()
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Database.Migration.Development.Plan.Data.AppPlans
import Wizard.Database.Migration.Development.Usage.Data.Usages
import Wizard.Service.App.AppMapper

instance ToSchema AppDetailDTO where
  declareNamedSchema =
    toSwagger (toDetailDTO defaultApp Nothing Nothing [standardPlan, standardPlanExpired] defaultUsage [])
