module Database.Migration.Development.Level.LevelMigration where

import Constant.Component
import Database.DAO.Level.LevelDAO
import Database.Migration.Development.Level.Data.Levels
import Util.Logger

runMigration = do
  logInfo $ msg _CMP_MIGRATION "(Level/Level) started"
  deleteLevels
  insertLevel level1
  insertLevel level2
  insertLevel level3
  logInfo $ msg _CMP_MIGRATION "(Level/Level) ended"
