module Wizard.Service.User.UserCommandExecutor where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Model.Context.AppContext
import Wizard.Service.User.UserService
import Wizard.Util.Logger
import WizardLib.Public.Model.PersistentCommand.User.CreateOrUpdateUserCommand
import WizardLib.Public.Model.PersistentCommand.User.DeleteUserCommand

cComponent = "user"

execute :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command.function == cCreateUserName = cCreateUser command
  | command.function == cUpdateUserName = cUpdateUser command
  | command.function == cDeleteUserName = cDeleteUser command

cCreateUserName = "createUser"

cCreateUser :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cCreateUser persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String CreateOrUpdateUserCommand
  case eCommand of
    Right command -> do
      createOrUpdateUserFromCommand command
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

cUpdateUserName = "updateUser"

cUpdateUser :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cUpdateUser persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String CreateOrUpdateUserCommand
  case eCommand of
    Right command -> do
      createOrUpdateUserFromCommand command
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

cDeleteUserName = "deleteUser"

cDeleteUser :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cDeleteUser persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String DeleteUserCommand
  case eCommand of
    Right command -> do
      deleteUser command.uuid
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])
