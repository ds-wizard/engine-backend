module Wizard.Database.Migration.Development.App.AppMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.App.AppDAO
import Wizard.Database.Migration.Development.App.Data.Apps

runMigration = do
  logInfo _CMP_MIGRATION "(App/App) started"
  deleteApps
  insertApp defaultApp
  insertApp differentApp
  logInfo _CMP_MIGRATION "(App/App) ended"
