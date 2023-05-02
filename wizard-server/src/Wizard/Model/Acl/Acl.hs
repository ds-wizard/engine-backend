module Wizard.Model.Acl.Acl where

import Data.Hashable
import qualified Data.UUID as U
import GHC.Generics

data Group = Group
  { gId :: String
  , name :: String
  , description :: String
  }
  deriving (Generic, Eq, Show)

data GroupMembership = GroupMembership
  { groupId :: String
  , gType :: GroupMembershipType
  }
  deriving (Generic, Eq, Show)

instance Hashable GroupMembership

data GroupMembershipType
  = OwnerGroupMembershipType
  | MemberGroupMembershipType
  deriving (Generic, Eq, Show, Read)

instance Hashable GroupMembershipType

data Member
  = GroupMember
      { gId :: String
      }
  | UserMember
      { uuid :: U.UUID
      }
  deriving (Generic, Eq, Show)

_ADMIN_PERM :: String
_ADMIN_PERM = "ADMIN"

_EDIT_PERM :: String
_EDIT_PERM = "EDIT"

_COMMENT_PERM :: String
_COMMENT_PERM = "COMMENT"

_VIEW_PERM :: String
_VIEW_PERM = "VIEW"
