module Wizard.Model.Questionnaire.QuestionnaireAclHelpers where

import qualified Data.UUID as U

import Wizard.Model.Acl.Acl
import Wizard.Model.Questionnaire.QuestionnairePerm

getUserUuidsForViewerPerm :: [QuestionnairePerm] -> [U.UUID]
getUserUuidsForViewerPerm = getUserUuidsForPerm _VIEW_PERM

getUserUuidsForCommentatorPerm :: [QuestionnairePerm] -> [U.UUID]
getUserUuidsForCommentatorPerm = getUserUuidsForPerm _COMMENT_PERM

getUserUuidsForEditorPerm :: [QuestionnairePerm] -> [U.UUID]
getUserUuidsForEditorPerm = getUserUuidsForPerm _EDIT_PERM

getUserUuidsForOwnerPerm :: [QuestionnairePerm] -> [U.UUID]
getUserUuidsForOwnerPerm = getUserUuidsForPerm _ADMIN_PERM

getUserUuidsForPerm :: String -> [QuestionnairePerm] -> [U.UUID]
getUserUuidsForPerm desiredPerm = foldl go []
  where
    go :: [U.UUID] -> QuestionnairePerm -> [U.UUID]
    go acc qtnPerm =
      case qtnPerm.memberType of
        UserQuestionnairePermType ->
          if desiredPerm `elem` qtnPerm.perms
            then acc ++ [qtnPerm.memberUuid]
            else acc
        _ -> acc

getUserGroupUuidsForViewerPerm :: [QuestionnairePerm] -> [U.UUID]
getUserGroupUuidsForViewerPerm = getUserGroupUuidsForPerm _VIEW_PERM

getUserGroupUuidsForCommentatorPerm :: [QuestionnairePerm] -> [U.UUID]
getUserGroupUuidsForCommentatorPerm = getUserGroupUuidsForPerm _COMMENT_PERM

getUserGroupUuidsForEditorPerm :: [QuestionnairePerm] -> [U.UUID]
getUserGroupUuidsForEditorPerm = getUserGroupUuidsForPerm _EDIT_PERM

getUserGroupUuidsForOwnerPerm :: [QuestionnairePerm] -> [U.UUID]
getUserGroupUuidsForOwnerPerm = getUserGroupUuidsForPerm _ADMIN_PERM

getUserGroupUuidsForPerm :: String -> [QuestionnairePerm] -> [U.UUID]
getUserGroupUuidsForPerm desiredPerm = foldl go []
  where
    go :: [U.UUID] -> QuestionnairePerm -> [U.UUID]
    go acc qtnPerm =
      case qtnPerm.memberType of
        UserGroupQuestionnairePermType ->
          if desiredPerm `elem` qtnPerm.perms
            then acc ++ [qtnPerm.memberUuid]
            else acc
        _ -> acc

removeUserPermission :: U.UUID -> [QuestionnairePerm] -> [QuestionnairePerm]
removeUserPermission userUuidToDelete = filter go
  where
    go :: QuestionnairePerm -> Bool
    go qtnPerm =
      case qtnPerm.memberType of
        UserQuestionnairePermType -> qtnPerm.memberUuid /= userUuidToDelete
        _ -> True
