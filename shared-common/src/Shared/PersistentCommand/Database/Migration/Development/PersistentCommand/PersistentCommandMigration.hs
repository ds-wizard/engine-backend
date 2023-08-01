module Shared.PersistentCommand.Database.Migration.Development.PersistentCommand.PersistentCommandMigration where

import Shared.Common.Constant.Component
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Logger
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO

runMigration :: AppContextC s sc m => m ()
runMigration = do
  logInfoI _CMP_MIGRATION "(PersistentCommand/PersistentCommand) started"
  deletePersistentCommands
  logInfoI _CMP_MIGRATION "(PersistentCommand/PersistentCommand) ended"
