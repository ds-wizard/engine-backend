module Wizard.Service.Questionnaire.Collaboration.CollaborationAcl where

import Control.Monad.Except (throwError)
import Data.Maybe (isJust)
import qualified Data.UUID as U
import Prelude hiding (log)

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAclHelpers
import Wizard.Model.Questionnaire.QuestionnairePerm
import Wizard.Model.User.User
import Wizard.Model.Websocket.WebsocketRecord

getPermission
  :: QuestionnaireVisibility
  -> QuestionnaireSharing
  -> [QuestionnairePerm]
  -> Maybe U.UUID
  -> Maybe String
  -> [U.UUID]
  -> WebsocketPerm
getPermission visibility sharing permissions mCurrentUserUuid mCurrentUserRole mCurrentUserGroupUuids
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
      [ isLogged && isExplicitlyCommenter
      , isLogged && isInCommenterGroup
      , isLogged && visibility == VisibleCommentQuestionnaire
      , sharing == AnyoneWithLinkCommentQuestionnaire
      ] =
      CommenterWebsocketPerm
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
    isExplicitlyCommenter = maybe False (`elem` getUserUuidsForCommenterPerm permissions) mCurrentUserUuid
    isExplicitlyViewer = maybe False (`elem` getUserUuidsForViewerPerm permissions) mCurrentUserUuid
    isInOwnerGroup = or (fmap (`elem` getUserGroupUuidsForOwnerPerm permissions) mCurrentUserGroupUuids)
    isInEditorGroup = or (fmap (`elem` getUserGroupUuidsForEditorPerm permissions) mCurrentUserGroupUuids)
    isInCommenterGroup = or (fmap (`elem` getUserGroupUuidsForCommenterPerm permissions) mCurrentUserGroupUuids)
    isInViewerGroup = or (fmap (`elem` getUserGroupUuidsForViewerPerm permissions) mCurrentUserGroupUuids)
    isLogged = isJust mCurrentUserUuid
    isAdmin = mCurrentUserRole == Just _USER_ROLE_ADMIN

checkViewPermission myself =
  if myself.entityPerm == EditorWebsocketPerm || myself.entityPerm == CommenterWebsocketPerm || myself.entityPerm == ViewerWebsocketPerm
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "View Questionnaire"

checkCommentPermission myself =
  if myself.entityPerm == EditorWebsocketPerm || myself.entityPerm == CommenterWebsocketPerm
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Comment Questionnaire"

checkEditPermission myself =
  if myself.entityPerm == EditorWebsocketPerm
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Edit Questionnaire"
