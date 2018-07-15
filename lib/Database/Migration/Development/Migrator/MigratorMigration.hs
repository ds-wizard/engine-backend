module Database.Migration.Development.Migrator.MigratorMigration where

import Database.DAO.Migrator.MigratorDAO
import Util.Logger

runMigration = do
  logInfo "MIGRATION (Migrator/Migrator): started"
  deleteMigratorStates
  logInfo "MIGRATION (Migrator/Migrator): ended"
