module Wizard.Model.Project.Acl.ProjectAclHelpers where

import qualified Data.UUID as U

import Wizard.Model.Acl.Acl
import Wizard.Model.Project.Acl.ProjectPerm

getUserUuidsForViewerPerm :: ProjectPermC projectPerm => [projectPerm] -> [U.UUID]
getUserUuidsForViewerPerm = getUserUuidsForPerm _VIEW_PERM

getUserUuidsForCommenterPerm :: ProjectPermC projectPerm => [projectPerm] -> [U.UUID]
getUserUuidsForCommenterPerm = getUserUuidsForPerm _COMMENT_PERM

getUserUuidsForEditorPerm :: ProjectPermC projectPerm => [projectPerm] -> [U.UUID]
getUserUuidsForEditorPerm = getUserUuidsForPerm _EDIT_PERM

getUserUuidsForOwnerPerm :: ProjectPermC projectPerm => [projectPerm] -> [U.UUID]
getUserUuidsForOwnerPerm = getUserUuidsForPerm _ADMIN_PERM

getUserUuidsForPerm :: ProjectPermC projectPerm => String -> [projectPerm] -> [U.UUID]
getUserUuidsForPerm desiredPerm = foldl go []
  where
    go :: ProjectPermC projectPerm => [U.UUID] -> projectPerm -> [U.UUID]
    go acc projectPerm =
      case projectPerm.memberType of
        UserProjectPermType ->
          if desiredPerm `elem` projectPerm.perms
            then acc ++ [projectPerm.memberUuid]
            else acc
        _ -> acc

getUserGroupUuidsForViewerPerm :: ProjectPermC projectPerm => [projectPerm] -> [U.UUID]
getUserGroupUuidsForViewerPerm = getUserGroupUuidsForPerm _VIEW_PERM

getUserGroupUuidsForCommenterPerm :: ProjectPermC projectPerm => [projectPerm] -> [U.UUID]
getUserGroupUuidsForCommenterPerm = getUserGroupUuidsForPerm _COMMENT_PERM

getUserGroupUuidsForEditorPerm :: ProjectPermC projectPerm => [projectPerm] -> [U.UUID]
getUserGroupUuidsForEditorPerm = getUserGroupUuidsForPerm _EDIT_PERM

getUserGroupUuidsForOwnerPerm :: ProjectPermC projectPerm => [projectPerm] -> [U.UUID]
getUserGroupUuidsForOwnerPerm = getUserGroupUuidsForPerm _ADMIN_PERM

getUserGroupUuidsForPerm :: ProjectPermC projectPerm => String -> [projectPerm] -> [U.UUID]
getUserGroupUuidsForPerm desiredPerm = foldl go []
  where
    go :: ProjectPermC projectPerm => [U.UUID] -> projectPerm -> [U.UUID]
    go acc projectPerm =
      case projectPerm.memberType of
        UserGroupProjectPermType ->
          if desiredPerm `elem` projectPerm.perms
            then acc ++ [projectPerm.memberUuid]
            else acc
        _ -> acc

removeUserPermission :: ProjectPermC projectPerm => U.UUID -> [projectPerm] -> [projectPerm]
removeUserPermission userUuidToDelete = filter go
  where
    go :: ProjectPermC projectPerm => projectPerm -> Bool
    go projectPerm =
      case projectPerm.memberType of
        UserProjectPermType -> projectPerm.memberUuid /= userUuidToDelete
        _ -> True
