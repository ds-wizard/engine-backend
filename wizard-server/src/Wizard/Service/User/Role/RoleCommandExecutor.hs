module Wizard.Service.User.Role.RoleCommandExecutor where

import Control.Monad.Except (throwError)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Shared.Common.Model.Error.Error
import Shared.Common.Util.Logger
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Model.Context.AppContext
import Wizard.Service.User.Role.RoleService
import WizardLib.Public.Model.PersistentCommand.User.CreateOrUpdateRoleCommand
import WizardLib.Public.Model.PersistentCommand.User.DeleteRoleCommand

cComponent = "role"

execute :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command.function == cCreateRoleName = cCreateRole command
  | command.function == cUpdateRoleName = cUpdateRole command
  | command.function == cDeleteRoleName = cDeleteRole command
  | otherwise = throwError . GeneralServerError $ "Unknown command function: " <> command.function

cCreateRoleName = "createRole"

cCreateRole :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cCreateRole persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String CreateOrUpdateRoleCommand
  case eCommand of
    Right command -> do
      createOrUpdateRoleFromCommand command
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

cUpdateRoleName = "updateRole"

cUpdateRole :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cUpdateRole persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String CreateOrUpdateRoleCommand
  case eCommand of
    Right command -> do
      createOrUpdateRoleFromCommand command
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

cDeleteRoleName = "deleteRole"

cDeleteRole :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cDeleteRole persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String DeleteRoleCommand
  case eCommand of
    Right command -> do
      deleteRoleFromCommand command.uuid
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])
