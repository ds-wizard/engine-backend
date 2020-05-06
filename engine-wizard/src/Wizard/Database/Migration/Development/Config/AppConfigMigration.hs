module Wizard.Database.Migration.Development.Config.AppConfigMigration where

import Wizard.Constant.Component
import Wizard.Database.DAO.Config.AppConfigDAO
import Wizard.Database.Migration.Development.Config.Data.AppConfigs
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(Config/AppConfig) started"
  deleteAppConfigs
  insertAppConfig defaultAppConfigEncrypted
  logInfo _CMP_MIGRATION "(Config/AppConfig) ended"
