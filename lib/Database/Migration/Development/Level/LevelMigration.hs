module Database.Migration.Development.Level.LevelMigration where

import Database.DAO.Level.LevelDAO
import Database.Migration.Development.Level.Data.Levels
import Util.Logger

runMigration = do
  logInfo "MIGRATION (Level/Level): started"
  deleteLevels
  insertLevel level1
  insertLevel level2
  insertLevel level3
  logInfo "MIGRATION (Level/Level): ended"
