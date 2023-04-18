module Registry.Database.Migration.Development.PersistentCommand.PersistentCommandMigration where

import Registry.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Common.Constant.Component

import Registry.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(PersistentCommand/PersistentCommand) started"
  deletePersistentCommands
  logInfo _CMP_MIGRATION "(PersistentCommand/PersistentCommand) ended"
