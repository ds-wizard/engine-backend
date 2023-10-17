module Wizard.Service.PersistentCommand.PersistentCommandExecutor where

import qualified Data.UUID as U

import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Model.Context.AppContext
import qualified Wizard.Service.Questionnaire.QuestionnaireCommandExecutor as QuestionnaireCommandExecutor
import qualified Wizard.Service.Tenant.Config.ConfigCommandExecutor as TenantConfigCommandExecutor
import qualified Wizard.Service.Tenant.Plan.PlanCommandExecutor as TenantPlanCommandExecutor
import qualified Wizard.Service.Tenant.TenantCommandExecutor as TenantCommandExecutor
import qualified Wizard.Service.User.Group.UserGroupCommandExecutor as UserGroupCommandExecutor
import qualified Wizard.Service.User.GroupMembership.UserGroupMembershipCommandExecutor as UserGroupMembershipCommandExecutor
import qualified Wizard.Service.User.UserCommandExecutor as UserCommandExecutor

execute :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command.component == QuestionnaireCommandExecutor.cComponent = QuestionnaireCommandExecutor.execute command
  | command.component == TenantCommandExecutor.cComponent = TenantCommandExecutor.execute command
  | command.component == TenantConfigCommandExecutor.cComponent = TenantConfigCommandExecutor.execute command
  | command.component == TenantPlanCommandExecutor.cComponent = TenantPlanCommandExecutor.execute command
  | command.component == UserCommandExecutor.cComponent = UserCommandExecutor.execute command
  | command.component == UserGroupCommandExecutor.cComponent = UserGroupCommandExecutor.execute command
  | command.component == UserGroupMembershipCommandExecutor.cComponent = UserGroupMembershipCommandExecutor.execute command
