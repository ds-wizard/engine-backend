module Wizard.Model.Questionnaire.QuestionnaireAclHelpers where

import Control.Lens ((^.))
import qualified Data.UUID as U
import LensesConfig
import Wizard.Model.Acl.Acl
import Wizard.Model.Questionnaire.QuestionnaireAcl

getUserUuidsForViewerPerm :: [QuestionnairePermRecord] -> [U.UUID]
getUserUuidsForViewerPerm = getUserUuidsForPerm _VIEW_PERM

getUserUuidsForEditorPerm :: [QuestionnairePermRecord] -> [U.UUID]
getUserUuidsForEditorPerm = getUserUuidsForPerm _EDIT_PERM

getUserUuidsForOwnerPerm :: [QuestionnairePermRecord] -> [U.UUID]
getUserUuidsForOwnerPerm = getUserUuidsForPerm _ADMIN_PERM

getUserUuidsForPerm :: String -> [QuestionnairePermRecord] -> [U.UUID]
getUserUuidsForPerm desiredPerm = foldl go []
  where
    go :: [U.UUID] -> QuestionnairePermRecord -> [U.UUID]
    go acc record =
      case record ^. member of
        UserMember {_userMemberUuid = userUuid} ->
          if desiredPerm `elem` (record ^. perms)
            then acc ++ [userUuid]
            else acc
        _ -> acc

getGroupIdsForViewerPerm :: [QuestionnairePermRecord] -> [String]
getGroupIdsForViewerPerm = getGroupIdsForPerm _VIEW_PERM

getGroupIdsForEditorPerm :: [QuestionnairePermRecord] -> [String]
getGroupIdsForEditorPerm = getGroupIdsForPerm _EDIT_PERM

getGroupIdsForOwnerPerm :: [QuestionnairePermRecord] -> [String]
getGroupIdsForOwnerPerm = getGroupIdsForPerm _ADMIN_PERM

getGroupIdsForPerm :: String -> [QuestionnairePermRecord] -> [String]
getGroupIdsForPerm desiredPerm = foldl go []
  where
    go :: [String] -> QuestionnairePermRecord -> [String]
    go acc record =
      case record ^. member of
        GroupMember {_groupMemberGId = groupId} ->
          if desiredPerm `elem` (record ^. perms)
            then acc ++ [groupId]
            else acc
        _ -> acc

removeUserPermission :: U.UUID -> [QuestionnairePermRecord] -> [QuestionnairePermRecord]
removeUserPermission userUuidToDelete = filter go
  where
    go :: QuestionnairePermRecord -> Bool
    go record =
      case record ^. member of
        UserMember {_userMemberUuid = userUuid} -> userUuid /= userUuidToDelete
        _ -> True
