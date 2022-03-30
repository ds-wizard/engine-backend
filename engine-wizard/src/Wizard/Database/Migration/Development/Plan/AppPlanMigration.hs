module Wizard.Database.Migration.Development.Plan.AppPlanMigration where

import Shared.Constant.Component
import Wizard.Database.DAO.Plan.AppPlanDAO
import Wizard.Database.Migration.Development.Plan.Data.AppPlans
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(App/Plan) started"
  deleteAppPlans
  insertAppPlan standardPlanExpired
  insertAppPlan standardPlan
  logInfo _CMP_MIGRATION "(App/Plan) ended"
