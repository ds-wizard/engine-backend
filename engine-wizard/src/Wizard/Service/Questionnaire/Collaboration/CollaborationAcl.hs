module Wizard.Service.Questionnaire.Collaboration.CollaborationAcl where

import Control.Monad.Except (throwError)
import Data.Maybe (isJust)
import qualified Data.UUID as U
import Prelude hiding (log)

import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Model.Acl.Acl
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAcl
import Wizard.Model.Questionnaire.QuestionnaireAclHelpers
import Wizard.Model.User.User
import Wizard.Model.Websocket.WebsocketRecord

getPermission
  :: QuestionnaireVisibility
  -> QuestionnaireSharing
  -> [QuestionnairePermRecord]
  -> Maybe U.UUID
  -> Maybe String
  -> Maybe [GroupMembership]
  -> WebsocketPerm
getPermission visibility sharing permissions mCurrentUserUuid mCurrentUserRole mCurrentUserGroups
  | or
      [ isAdmin
      , isLogged && isExplicitlyOwner
      , isLogged && isExplicitlyEditor
      , isLogged && isInOwnerGroup
      , isLogged && isInEditorGroup
      , isLogged && visibility == VisibleEditQuestionnaire
      , sharing == AnyoneWithLinkEditQuestionnaire
      ] =
      EditorWebsocketPerm
  | or
      [ isLogged && isExplicitlyCommentator
      , isLogged && isInCommentatorGroup
      , isLogged && visibility == VisibleCommentQuestionnaire
      , sharing == AnyoneWithLinkCommentQuestionnaire
      ] =
      CommentatorWebsocketPerm
  | or
      [ isLogged && isExplicitlyViewer
      , isLogged && isInViewerGroup
      , isLogged && visibility == VisibleViewQuestionnaire
      , sharing == AnyoneWithLinkViewQuestionnaire
      ] =
      ViewerWebsocketPerm
  | otherwise = NoWebsocketPerm
  where
    isExplicitlyOwner = maybe False (`elem` getUserUuidsForOwnerPerm permissions) mCurrentUserUuid
    isExplicitlyEditor = maybe False (`elem` getUserUuidsForEditorPerm permissions) mCurrentUserUuid
    isExplicitlyCommentator = maybe False (`elem` getUserUuidsForCommentatorPerm permissions) mCurrentUserUuid
    isExplicitlyViewer = maybe False (`elem` getUserUuidsForViewerPerm permissions) mCurrentUserUuid
    isInOwnerGroup = or (fmap (`elem` getGroupIdsForOwnerPerm permissions) mCurrentUserGroupIds)
    isInEditorGroup = or (fmap (`elem` getGroupIdsForEditorPerm permissions) mCurrentUserGroupIds)
    isInCommentatorGroup = or (fmap (`elem` getGroupIdsForCommentatorPerm permissions) mCurrentUserGroupIds)
    isInViewerGroup = or (fmap (`elem` getGroupIdsForViewerPerm permissions) mCurrentUserGroupIds)
    isLogged = isJust mCurrentUserUuid
    isAdmin = mCurrentUserRole == Just _USER_ROLE_ADMIN
    mCurrentUserGroupIds = maybe [] (fmap (.groupId)) mCurrentUserGroups

checkViewPermission myself =
  if myself.entityPerm == EditorWebsocketPerm || myself.entityPerm == CommentatorWebsocketPerm || myself.entityPerm == ViewerWebsocketPerm
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "View Questionnaire"

checkCommentPermission myself =
  if myself.entityPerm == EditorWebsocketPerm || myself.entityPerm == CommentatorWebsocketPerm
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Comment Questionnaire"

checkEditPermission myself =
  if myself.entityPerm == EditorWebsocketPerm
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Edit Questionnaire"
