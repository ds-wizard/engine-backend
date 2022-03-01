module Wizard.Database.Migration.Development.Limit.AppLimitMigration where

import Shared.Constant.Component
import Wizard.Database.DAO.App.AppLimitDAO
import Wizard.Database.Migration.Development.Limit.Data.AppLimits
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(App/Limit) started"
  deleteAppLimits
  insertAppLimit defaultAppLimit
  insertAppLimit differentAppLimit
  logInfo _CMP_MIGRATION "(App/Limit) ended"
