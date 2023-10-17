module WizardLib.Public.Model.User.UserGroupMembership where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data UserGroupMembershipType
  = OwnerUserGroupMembershipType
  | MemberUserGroupMembershipType
  deriving (Generic, Eq, Show, Read)

data UserGroupMembership = UserGroupMembership
  { userGroupUuid :: U.UUID
  , userUuid :: U.UUID
  , mType :: UserGroupMembershipType
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Eq, Show)
