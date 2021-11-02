module Wizard.Database.Migration.Development.App.AppMigration where

import Shared.Constant.Component
import Wizard.Database.DAO.App.AppDAO
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(App/App) started"
  deleteApps
  insertApp defaultApp
  insertApp differentApp
  logInfo _CMP_MIGRATION "(App/App) ended"
