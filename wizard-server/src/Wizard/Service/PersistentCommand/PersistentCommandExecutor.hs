module Wizard.Service.PersistentCommand.PersistentCommandExecutor where

import qualified Data.UUID as U

import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Model.Context.AppContext
import qualified Wizard.Service.Tenant.Config.ConfigCommandExecutor as TenantConfigCommandExecutor
import qualified Wizard.Service.Tenant.Plan.PlanCommandExecutor as TenantPlanCommandExecutor
import qualified Wizard.Service.Tenant.TenantCommandExecutor as AppCommandExecutor
import qualified Wizard.Service.User.UserCommandExecutor as UserCommandExecutor

execute :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command.component == AppCommandExecutor.cComponent = AppCommandExecutor.execute command
  | command.component == TenantConfigCommandExecutor.cComponent = TenantConfigCommandExecutor.execute command
  | command.component == TenantPlanCommandExecutor.cComponent = TenantPlanCommandExecutor.execute command
  | command.component == UserCommandExecutor.cComponent = UserCommandExecutor.execute command
