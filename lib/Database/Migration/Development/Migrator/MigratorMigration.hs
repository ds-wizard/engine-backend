module Database.Migration.Development.Migrator.MigratorMigration where

import Constant.Component
import Database.DAO.Migrator.MigratorDAO
import Util.Logger

runMigration = do
  logInfo $ msg _CMP_MIGRATION "(Migrator/Migrator) started"
  deleteMigratorStates
  logInfo $ msg _CMP_MIGRATION "(Migrator/Migrator) ended"
