module Wizard.Service.PersistentCommand.PersistentCommandExecutor where

import Wizard.Model.Context.AppContext
import Wizard.Model.PersistentCommand.PersistentCommand
import qualified Wizard.Service.App.AppCommandExecutor as AppCommandExecutor
import qualified Wizard.Service.Config.App.AppConfigCommandExecutor as AppConfigCommandExecutor

execute :: PersistentCommand -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command.component == AppConfigCommandExecutor.cComponent = AppConfigCommandExecutor.execute command
  | command.component == AppCommandExecutor.cComponent = AppCommandExecutor.execute command
