module Wizard.Model.Questionnaire.QuestionnaireAclHelpers where

import qualified Data.UUID as U

import Wizard.Model.Acl.Acl
import Wizard.Model.Questionnaire.QuestionnairePerm

getUserUuidsForViewerPerm :: QuestionnairePermC questionnairePerm => [questionnairePerm] -> [U.UUID]
getUserUuidsForViewerPerm = getUserUuidsForPerm _VIEW_PERM

getUserUuidsForCommenterPerm :: QuestionnairePermC questionnairePerm => [questionnairePerm] -> [U.UUID]
getUserUuidsForCommenterPerm = getUserUuidsForPerm _COMMENT_PERM

getUserUuidsForEditorPerm :: QuestionnairePermC questionnairePerm => [questionnairePerm] -> [U.UUID]
getUserUuidsForEditorPerm = getUserUuidsForPerm _EDIT_PERM

getUserUuidsForOwnerPerm :: QuestionnairePermC questionnairePerm => [questionnairePerm] -> [U.UUID]
getUserUuidsForOwnerPerm = getUserUuidsForPerm _ADMIN_PERM

getUserUuidsForPerm :: QuestionnairePermC questionnairePerm => String -> [questionnairePerm] -> [U.UUID]
getUserUuidsForPerm desiredPerm = foldl go []
  where
    go :: QuestionnairePermC questionnairePerm => [U.UUID] -> questionnairePerm -> [U.UUID]
    go acc qtnPerm =
      case qtnPerm.memberType of
        UserQuestionnairePermType ->
          if desiredPerm `elem` qtnPerm.perms
            then acc ++ [qtnPerm.memberUuid]
            else acc
        _ -> acc

getUserGroupUuidsForViewerPerm :: QuestionnairePermC questionnairePerm => [questionnairePerm] -> [U.UUID]
getUserGroupUuidsForViewerPerm = getUserGroupUuidsForPerm _VIEW_PERM

getUserGroupUuidsForCommenterPerm :: QuestionnairePermC questionnairePerm => [questionnairePerm] -> [U.UUID]
getUserGroupUuidsForCommenterPerm = getUserGroupUuidsForPerm _COMMENT_PERM

getUserGroupUuidsForEditorPerm :: QuestionnairePermC questionnairePerm => [questionnairePerm] -> [U.UUID]
getUserGroupUuidsForEditorPerm = getUserGroupUuidsForPerm _EDIT_PERM

getUserGroupUuidsForOwnerPerm :: QuestionnairePermC questionnairePerm => [questionnairePerm] -> [U.UUID]
getUserGroupUuidsForOwnerPerm = getUserGroupUuidsForPerm _ADMIN_PERM

getUserGroupUuidsForPerm :: QuestionnairePermC questionnairePerm => String -> [questionnairePerm] -> [U.UUID]
getUserGroupUuidsForPerm desiredPerm = foldl go []
  where
    go :: QuestionnairePermC questionnairePerm => [U.UUID] -> questionnairePerm -> [U.UUID]
    go acc qtnPerm =
      case qtnPerm.memberType of
        UserGroupQuestionnairePermType ->
          if desiredPerm `elem` qtnPerm.perms
            then acc ++ [qtnPerm.memberUuid]
            else acc
        _ -> acc

removeUserPermission :: QuestionnairePermC questionnairePerm => U.UUID -> [questionnairePerm] -> [questionnairePerm]
removeUserPermission userUuidToDelete = filter go
  where
    go :: QuestionnairePermC questionnairePerm => questionnairePerm -> Bool
    go qtnPerm =
      case qtnPerm.memberType of
        UserQuestionnairePermType -> qtnPerm.memberUuid /= userUuidToDelete
        _ -> True
