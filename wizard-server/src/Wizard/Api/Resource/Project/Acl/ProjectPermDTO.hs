module Wizard.Api.Resource.Project.Acl.ProjectPermDTO where

import qualified Data.UUID as U
import GHC.Generics
import GHC.Records

import Wizard.Api.Resource.Acl.MemberDTO
import Wizard.Model.Project.Acl.ProjectPerm

data ProjectPermDTO = ProjectPermDTO
  { projectUuid :: U.UUID
  , member :: MemberDTO
  , perms :: [String]
  }
  deriving (Generic, Eq, Show)

instance Ord ProjectPermDTO where
  compare a b = compare a.member.uuid b.member.uuid

instance ProjectPermC ProjectPermDTO

instance HasField "memberType" ProjectPermDTO ProjectPermType where
  getField perm =
    case perm.member of
      UserMemberDTO {} -> UserProjectPermType
      UserGroupMemberDTO {} -> UserGroupProjectPermType

instance HasField "memberUuid" ProjectPermDTO U.UUID where
  getField perm = perm.member.uuid
