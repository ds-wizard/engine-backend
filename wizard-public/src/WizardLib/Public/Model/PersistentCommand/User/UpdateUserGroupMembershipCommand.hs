module WizardLib.Public.Model.PersistentCommand.User.UpdateUserGroupMembershipCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson
import WizardLib.Public.Api.Resource.User.UserGroupMembershipJM ()
import WizardLib.Public.Model.User.UserGroupMembership

data UpdateUserGroupMembershipCommand = UpdateUserGroupMembershipCommand
  { userGroupUuid :: U.UUID
  , members :: [UpdateUserGroupMembershipMember]
  }
  deriving (Show, Eq, Generic)

data UpdateUserGroupMembershipMember = UpdateUserGroupMembershipMember
  { userUuid :: U.UUID
  , mType :: UserGroupMembershipType
  }
  deriving (Show, Generic)

instance Eq UpdateUserGroupMembershipMember where
  a == b = a.userUuid == b.userUuid

instance FromJSON UpdateUserGroupMembershipCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UpdateUserGroupMembershipCommand where
  toJSON = genericToJSON jsonOptions

instance FromJSON UpdateUserGroupMembershipMember where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UpdateUserGroupMembershipMember where
  toJSON = genericToJSON jsonOptions
