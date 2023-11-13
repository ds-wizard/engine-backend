module Wizard.Service.User.Group.UserGroupCommandExecutor where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Shared.Common.Util.Logger
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Model.Context.AppContext
import Wizard.Service.User.Group.UserGroupService
import WizardLib.Public.Model.PersistentCommand.User.CreateOrUpdateUserGroupCommand
import WizardLib.Public.Model.PersistentCommand.User.DeleteUserGroupCommand

cComponent = "user_group"

execute :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command.function == cCreateUserGroupName = cCreateUserGroup command
  | command.function == cUpdateUserGroupName = cUpdateUserGroup command
  | command.function == cDeleteUserGroupName = cDeleteUserGroup command

cCreateUserGroupName = "createUserGroup"

cCreateUserGroup :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cCreateUserGroup persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String CreateOrUpdateUserGroupCommand
  case eCommand of
    Right command -> do
      createUserGroup command.uuid command.name command.description command.private
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

cUpdateUserGroupName = "updateUserGroup"

cUpdateUserGroup :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cUpdateUserGroup persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String CreateOrUpdateUserGroupCommand
  case eCommand of
    Right command -> do
      modifyUserGroup command.uuid command.name command.description command.private
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

cDeleteUserGroupName = "deleteUserGroup"

cDeleteUserGroup :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cDeleteUserGroup persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String DeleteUserGroupCommand
  case eCommand of
    Right command -> do
      deleteUserGroup command.uuid
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])
