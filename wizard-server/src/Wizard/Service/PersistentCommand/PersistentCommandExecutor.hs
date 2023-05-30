module Wizard.Service.PersistentCommand.PersistentCommandExecutor where

import qualified Data.UUID as U

import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Model.Context.AppContext
import qualified Wizard.Service.App.AppCommandExecutor as AppCommandExecutor
import qualified Wizard.Service.Config.App.AppConfigCommandExecutor as AppConfigCommandExecutor

execute :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command.component == AppConfigCommandExecutor.cComponent = AppConfigCommandExecutor.execute command
  | command.component == AppCommandExecutor.cComponent = AppCommandExecutor.execute command
