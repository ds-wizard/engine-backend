module Wizard.Service.User.GroupMembership.UserGroupMembershipCommandExecutor where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Shared.Common.Util.Logger
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Model.Context.AppContext
import Wizard.Service.User.GroupMembership.UserGroupMembershipService
import WizardLib.Public.Model.PersistentCommand.User.UpdateUserGroupMembershipCommand

cComponent = "userGroupMembership"

execute :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command.function == cUpdateUserGroupMembershipName = cUpdateUserGroupMembership command

cUpdateUserGroupMembershipName = "updateUserGroupMembership"

cUpdateUserGroupMembership :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cUpdateUserGroupMembership persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String UpdateUserGroupMembershipCommand
  case eCommand of
    Right command -> do
      modifyUserGroupMemberships command
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])
