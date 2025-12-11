module Wizard.Model.Project.Acl.ProjectPerm where

import qualified Data.UUID as U
import GHC.Generics
import GHC.Records

import Wizard.Model.Acl.Acl

data ProjectPermType
  = UserProjectPermType
  | UserGroupProjectPermType
  deriving (Show, Eq, Generic, Read)

data ProjectPerm = ProjectPerm
  { projectUuid :: U.UUID
  , memberType :: ProjectPermType
  , memberUuid :: U.UUID
  , perms :: [String]
  , tenantUuid :: U.UUID
  }
  deriving (Generic, Eq, Show)

class
  ( HasField "perms" projectPerm [String]
  , HasField "memberUuid" projectPerm U.UUID
  , HasField "memberType" projectPerm ProjectPermType
  ) =>
  ProjectPermC projectPerm

instance ProjectPermC ProjectPerm

ownerPermissions = [_VIEW_PERM, _COMMENT_PERM, _EDIT_PERM, _ADMIN_PERM]

editorPermissions = [_VIEW_PERM, _COMMENT_PERM, _EDIT_PERM]

commentatorPermissions = [_VIEW_PERM, _COMMENT_PERM]

viewerPermissions = [_VIEW_PERM]
