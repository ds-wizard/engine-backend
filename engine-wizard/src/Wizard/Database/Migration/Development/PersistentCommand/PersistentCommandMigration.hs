module Wizard.Database.Migration.Development.PersistentCommand.PersistentCommandMigration where

import Shared.Constant.Component
import Wizard.Database.DAO.PersistentCommand.PersistentCommandDAO

--import Wizard.Database.Migration.Development.PersistentCommand.Data.PersistentCommands
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(PersistentCommand/PersistentCommand) started"
  deletePersistentCommands
--  insertPersistentCommand submission1
--  insertPersistentCommand differentPersistentCommand
  logInfo _CMP_MIGRATION "(PersistentCommand/PersistentCommand) ended"
