module Database.Migration.Development.Migrator.MigratorMigration where

import Control.Monad.Logger (logInfo)

import Database.DAO.Migrator.MigratorDAO

runMigration = do
  $(logInfo) "MIGRATION (Migrator/Migrator): started"
  deleteMigratorStates
  $(logInfo) "MIGRATION (Migrator/Migrator): ended"
