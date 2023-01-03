module Wizard.Database.Migration.Development.Config.ConfigMigration where

import Shared.Constant.Component
import Wizard.Database.DAO.Config.AppConfigDAO
import Wizard.Database.Migration.Development.Config.Data.AppConfigs
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(Config/Config) started"
  deleteAppConfigs
  insertAppConfig defaultAppConfigEncrypted
  insertAppConfig differentAppConfigEncrypted
  logInfo _CMP_MIGRATION "(Config/Config) ended"
