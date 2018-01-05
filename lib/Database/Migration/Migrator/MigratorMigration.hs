module Database.Migration.Migrator.MigratorMigration where

import Common.Context
import Database.DAO.Migrator.MigratorDAO

runMigration context dspConfig logState = do
  logState "MIGRATION (Migrator/Migrator): started"
  deleteMigratorStates context
  logState "MIGRATION (Migrator/Migrator): ended"
