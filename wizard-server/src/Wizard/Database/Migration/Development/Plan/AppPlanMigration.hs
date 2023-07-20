module Wizard.Database.Migration.Development.Plan.AppPlanMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.Plan.AppPlanDAO
import Wizard.Database.Migration.Development.Plan.Data.AppPlans

runMigration = do
  logInfo _CMP_MIGRATION "(App/Plan) started"
  deleteAppPlans
  insertAppPlan standardPlanExpired
  insertAppPlan standardPlan
  logInfo _CMP_MIGRATION "(App/Plan) ended"
