module Wizard.Service.Project.Collaboration.ProjectCollaborationAcl where

import Control.Monad.Except (throwError)
import Data.Maybe (isJust)
import qualified Data.UUID as U
import Prelude hiding (log)

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Model.Project.Acl.ProjectAclHelpers
import Wizard.Model.Project.Acl.ProjectPerm
import Wizard.Model.Project.Project
import Wizard.Model.User.User
import Wizard.Model.Websocket.WebsocketRecord

getPermission
  :: ProjectVisibility
  -> ProjectSharing
  -> [ProjectPerm]
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
      , isLogged && visibility == VisibleEditProjectVisibility
      , sharing == AnyoneWithLinkEditProjectSharing
      ] =
      EditorWebsocketPerm
  | or
      [ isLogged && isExplicitlyCommenter
      , isLogged && isInCommenterGroup
      , isLogged && visibility == VisibleCommentProjectVisibility
      , sharing == AnyoneWithLinkCommentProjectSharing
      ] =
      CommenterWebsocketPerm
  | or
      [ isLogged && isExplicitlyViewer
      , isLogged && isInViewerGroup
      , isLogged && visibility == VisibleViewProjectVisibility
      , sharing == AnyoneWithLinkViewProjectSharing
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

checkViewPermission entityPerm =
  if entityPerm == EditorWebsocketPerm || entityPerm == CommenterWebsocketPerm || entityPerm == ViewerWebsocketPerm
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "View Project"

checkCommentPermission entityPerm =
  if entityPerm == EditorWebsocketPerm || entityPerm == CommenterWebsocketPerm
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Comment Project"

checkEditPermission entityPerm =
  if entityPerm == EditorWebsocketPerm
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Edit Project"
