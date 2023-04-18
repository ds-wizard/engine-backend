module Wizard.Model.Questionnaire.QuestionnaireAclHelpers where

import Control.Monad.Reader (liftIO)
import qualified Data.UUID as U

import Shared.Common.Util.Uuid
import Wizard.Model.Acl.Acl
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.QuestionnaireAcl

getUserUuidsForViewerPerm :: [QuestionnairePermRecord] -> [U.UUID]
getUserUuidsForViewerPerm = getUserUuidsForPerm _VIEW_PERM

getUserUuidsForCommentatorPerm :: [QuestionnairePermRecord] -> [U.UUID]
getUserUuidsForCommentatorPerm = getUserUuidsForPerm _COMMENT_PERM

getUserUuidsForEditorPerm :: [QuestionnairePermRecord] -> [U.UUID]
getUserUuidsForEditorPerm = getUserUuidsForPerm _EDIT_PERM

getUserUuidsForOwnerPerm :: [QuestionnairePermRecord] -> [U.UUID]
getUserUuidsForOwnerPerm = getUserUuidsForPerm _ADMIN_PERM

getUserUuidsForPerm :: String -> [QuestionnairePermRecord] -> [U.UUID]
getUserUuidsForPerm desiredPerm = foldl go []
  where
    go :: [U.UUID] -> QuestionnairePermRecord -> [U.UUID]
    go acc record =
      case record.member of
        UserMember {uuid = userUuid} ->
          if desiredPerm `elem` record.perms
            then acc ++ [userUuid]
            else acc
        _ -> acc

getGroupIdsForViewerPerm :: [QuestionnairePermRecord] -> [String]
getGroupIdsForViewerPerm = getGroupIdsForPerm _VIEW_PERM

getGroupIdsForCommentatorPerm :: [QuestionnairePermRecord] -> [String]
getGroupIdsForCommentatorPerm = getGroupIdsForPerm _COMMENT_PERM

getGroupIdsForEditorPerm :: [QuestionnairePermRecord] -> [String]
getGroupIdsForEditorPerm = getGroupIdsForPerm _EDIT_PERM

getGroupIdsForOwnerPerm :: [QuestionnairePermRecord] -> [String]
getGroupIdsForOwnerPerm = getGroupIdsForPerm _ADMIN_PERM

getGroupIdsForPerm :: String -> [QuestionnairePermRecord] -> [String]
getGroupIdsForPerm desiredPerm = foldl go []
  where
    go :: [String] -> QuestionnairePermRecord -> [String]
    go acc record =
      case record.member of
        GroupMember {gId = groupId} ->
          if desiredPerm `elem` record.perms
            then acc ++ [groupId]
            else acc
        _ -> acc

duplicateUserPermission :: U.UUID -> QuestionnairePermRecord -> AppContextM QuestionnairePermRecord
duplicateUserPermission newQtnUuid record = do
  newUuid <- liftIO generateUuid
  return record {uuid = newUuid, questionnaireUuid = newQtnUuid}

removeUserPermission :: U.UUID -> [QuestionnairePermRecord] -> [QuestionnairePermRecord]
removeUserPermission userUuidToDelete = filter go
  where
    go :: QuestionnairePermRecord -> Bool
    go record =
      case record.member of
        UserMember {uuid = userUuid} -> userUuid /= userUuidToDelete
        _ -> True
