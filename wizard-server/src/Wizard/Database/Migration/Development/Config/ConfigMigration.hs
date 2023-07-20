module Wizard.Database.Migration.Development.Config.ConfigMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.Config.AppConfigDAO
import Wizard.Database.Migration.Development.Config.Data.AppConfigs

runMigration = do
  logInfo _CMP_MIGRATION "(Config/Config) started"
  deleteAppConfigs
  insertAppConfig defaultAppConfigEncrypted
  insertAppConfig differentAppConfigEncrypted
  logInfo _CMP_MIGRATION "(Config/Config) ended"
