module Wizard.Database.Migration.Development.Level.LevelMigration where

import Shared.Constant.Component
import Wizard.Database.DAO.Level.LevelDAO
import Wizard.Database.Migration.Development.Level.Data.Levels
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(Level/Level) started"
  deleteLevels
  insertLevel level1
  insertLevel level2
  insertLevel level3
  logInfo _CMP_MIGRATION "(Level/Level) ended"
