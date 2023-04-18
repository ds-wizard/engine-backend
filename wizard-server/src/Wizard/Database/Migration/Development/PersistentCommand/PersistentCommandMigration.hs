module Wizard.Database.Migration.Development.PersistentCommand.PersistentCommandMigration where

import Shared.Common.Constant.Component
import Wizard.Database.DAO.PersistentCommand.PersistentCommandDAO

import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(PersistentCommand/PersistentCommand) started"
  deletePersistentCommands
  logInfo _CMP_MIGRATION "(PersistentCommand/PersistentCommand) ended"
