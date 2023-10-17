module Wizard.Service.User.GroupMembership.UserGroupMembershipMapper where

import Data.Time
import qualified Data.UUID as U

import WizardLib.Public.Model.PersistentCommand.User.UpdateUserGroupMembershipCommand
import WizardLib.Public.Model.User.UserGroupMembership

toCreateDTO :: UserGroupMembership -> UpdateUserGroupMembershipMember
toCreateDTO membership =
  UpdateUserGroupMembershipMember
    { userUuid = membership.userUuid
    , mType = membership.mType
    }

fromCreateDTO :: UpdateUserGroupMembershipMember -> U.UUID -> U.UUID -> UTCTime -> UserGroupMembership
fromCreateDTO dto userGroupUuid tenantUuid now =
  UserGroupMembership
    { userGroupUuid = userGroupUuid
    , userUuid = dto.userUuid
    , mType = dto.mType
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    }

fromChangeDTO :: UserGroupMembership -> UpdateUserGroupMembershipMember -> UTCTime -> UserGroupMembership
fromChangeDTO membership dto now =
  UserGroupMembership
    { userGroupUuid = membership.userGroupUuid
    , userUuid = dto.userUuid
    , mType = dto.mType
    , tenantUuid = membership.tenantUuid
    , createdAt = membership.createdAt
    , updatedAt = now
    }
