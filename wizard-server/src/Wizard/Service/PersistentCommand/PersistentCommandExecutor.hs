module Wizard.Service.PersistentCommand.PersistentCommandExecutor where

import qualified Data.UUID as U

import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Model.Context.AppContext
import qualified Wizard.Service.App.AppCommandExecutor as AppCommandExecutor
import qualified Wizard.Service.Config.App.AppConfigCommandExecutor as AppConfigCommandExecutor
import qualified Wizard.Service.Plan.AppPlanCommandExecutor as AppPlanCommandExecutor
import qualified Wizard.Service.User.UserCommandExecutor as UserCommandExecutor

execute :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command.component == AppCommandExecutor.cComponent = AppCommandExecutor.execute command
  | command.component == AppConfigCommandExecutor.cComponent = AppConfigCommandExecutor.execute command
  | command.component == AppPlanCommandExecutor.cComponent = AppPlanCommandExecutor.execute command
  | command.component == UserCommandExecutor.cComponent = UserCommandExecutor.execute command
