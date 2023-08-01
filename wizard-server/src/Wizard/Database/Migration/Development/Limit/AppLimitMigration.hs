module Wizard.Database.Migration.Development.Limit.AppLimitMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.Limit.AppLimitDAO
import Wizard.Database.Migration.Development.Limit.Data.AppLimits

runMigration = do
  logInfo _CMP_MIGRATION "(App/Limit) started"
  deleteAppLimits
  insertAppLimit defaultAppLimit
  insertAppLimit differentAppLimit
  logInfo _CMP_MIGRATION "(App/Limit) ended"
