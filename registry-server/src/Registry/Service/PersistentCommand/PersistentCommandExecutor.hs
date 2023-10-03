module Registry.Service.PersistentCommand.PersistentCommandExecutor where

import qualified Data.UUID as U

import Registry.Model.Context.AppContext
import qualified Registry.Service.Organization.OrganizationCommandExecutor as OrganizationCommandExecutor
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand

execute :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command.component == OrganizationCommandExecutor.cComponent = OrganizationCommandExecutor.execute command
