module Wizard.Service.PersistentCommand.PersistentCommandExecutor where

import Control.Monad.Except (throwError)
import qualified Data.UUID as U

import Shared.Common.Model.Error.Error
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import qualified Shared.Prefab.Service.Prefab.PrefabCommandExecutor as PrefabCommandExecutor
import Wizard.Model.Context.AppContext
import qualified Wizard.Service.Locale.LocaleCommandExecutor as LocaleCommandExecutor
import qualified Wizard.Service.Migration.Metamodel.MigratorCommandExecutor as MetamodeMigratorCommandExecutor
import qualified Wizard.Service.Questionnaire.File.QuestionnaireFileCommandExecutor as QuestionnaireFileCommandExecutor
import qualified Wizard.Service.Questionnaire.QuestionnaireCommandExecutor as QuestionnaireCommandExecutor
import qualified Wizard.Service.Tenant.Config.ConfigCommandExecutor as TenantConfigCommandExecutor
import qualified Wizard.Service.Tenant.TenantCommandExecutor as TenantCommandExecutor
import qualified Wizard.Service.User.Group.UserGroupCommandExecutor as UserGroupCommandExecutor
import qualified Wizard.Service.User.GroupMembership.UserGroupMembershipCommandExecutor as UserGroupMembershipCommandExecutor
import qualified Wizard.Service.User.Tour.TourCommandExecutor as TourCommandExecutor
import qualified Wizard.Service.User.UserCommandExecutor as UserCommandExecutor

execute :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command.component == LocaleCommandExecutor.cComponent = LocaleCommandExecutor.execute command
  | command.component == MetamodeMigratorCommandExecutor.cComponent = MetamodeMigratorCommandExecutor.execute command
  | command.component == PrefabCommandExecutor.cComponent = PrefabCommandExecutor.execute command
  | command.component == QuestionnaireCommandExecutor.cComponent = QuestionnaireCommandExecutor.execute command
  | command.component == QuestionnaireFileCommandExecutor.cComponent = QuestionnaireFileCommandExecutor.execute command
  | command.component == TenantCommandExecutor.cComponent = TenantCommandExecutor.execute command
  | command.component == TenantConfigCommandExecutor.cComponent = TenantConfigCommandExecutor.execute command
  | command.component == TourCommandExecutor.cComponent = TourCommandExecutor.execute command
  | command.component == UserCommandExecutor.cComponent = UserCommandExecutor.execute command
  | command.component == UserGroupCommandExecutor.cComponent = UserGroupCommandExecutor.execute command
  | command.component == UserGroupMembershipCommandExecutor.cComponent = UserGroupMembershipCommandExecutor.execute command
  | otherwise = throwError . GeneralServerError $ "Unknown command component: " <> command.component
