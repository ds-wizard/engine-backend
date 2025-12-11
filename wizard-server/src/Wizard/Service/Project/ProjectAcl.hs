module Wizard.Service.Project.ProjectAcl where

import Control.Monad (unless)
import Control.Monad.Except (throwError)

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Project.Acl.ProjectAclHelpers
import Wizard.Model.Project.Acl.ProjectPerm
import Wizard.Model.Project.Project
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.Tenant.Config.ConfigService
import WizardLib.Public.Database.DAO.User.UserGroupMembershipDAO
import WizardLib.Public.Model.User.UserGroupMembership

checkCreatePermissionToProject :: AppContextM ()
checkCreatePermissionToProject = do
  tcProject <- getCurrentTenantConfigProject
  let projectSharingEnabled = tcProject.projectSharing.enabled
  let projectSharingAnonymousEnabled = tcProject.projectSharing.anonymousEnabled
  let projectCreation = tcProject.projectCreation
  case (projectSharingEnabled, projectSharingAnonymousEnabled, projectCreation) of
    (True, True, CustomProjectCreation) -> return ()
    (True, True, TemplateAndCustomProjectCreation) -> return ()
    (_, _, TemplateProjectCreation) -> do
      checkPermission _PRJ_PERM
      checkPermission _PRJ_TML_PERM
    (_, _, _) -> checkPermission _PRJ_PERM

checkCreateFromTemplatePermissionToProject :: Bool -> AppContextM ()
checkCreateFromTemplatePermissionToProject isTemplate = do
  checkPermission _PRJ_PERM
  tcProject <- getCurrentTenantConfigProject
  let projectCreation = tcProject.projectCreation
  case projectCreation of
    CustomProjectCreation ->
      throwError . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Project Template"
    _ -> unless isTemplate (throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Project Template")

checkClonePermissionToProject :: ProjectPermC projectPerm => ProjectVisibility -> ProjectSharing -> [projectPerm] -> AppContextM ()
checkClonePermissionToProject visibility sharing permissions = do
  checkPermission _PRJ_PERM
  checkViewPermissionToProject visibility sharing permissions

checkViewPermissionToProject :: ProjectPermC projectPerm => ProjectVisibility -> ProjectSharing -> [projectPerm] -> AppContextM ()
checkViewPermissionToProject visibility sharing perms = do
  result <- hasViewPermissionToProject visibility sharing perms
  if result
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "View Project"

hasViewPermissionToProject :: ProjectPermC projectPerm => ProjectVisibility -> ProjectSharing -> [projectPerm] -> AppContextM Bool
hasViewPermissionToProject visibility sharing perms =
  if sharing == AnyoneWithLinkViewProjectSharing
    || sharing == AnyoneWithLinkCommentProjectSharing
    || sharing
      == AnyoneWithLinkEditProjectSharing
    then return True
    else do
      checkPermission _PRJ_PERM
      currentUser <- getCurrentUser
      userGroupMemberships <- findUserGroupMembershipsByUserUuid currentUser.uuid
      let currentUserGroupUuids = fmap (.userGroupUuid) userGroupMemberships
      if or
        [ currentUser.uRole == _USER_ROLE_ADMIN
        , -- Check visibility
          visibility == VisibleViewProjectVisibility
        , visibility == VisibleCommentProjectVisibility
        , visibility == VisibleEditProjectVisibility
        , -- Check membership
          currentUser.uuid `elem` getUserUuidsForViewerPerm perms
        , currentUser.uuid `elem` getUserUuidsForCommenterPerm perms
        , currentUser.uuid `elem` getUserUuidsForEditorPerm perms
        , currentUser.uuid `elem` getUserUuidsForOwnerPerm perms
        , -- Check groups
          or (fmap (`elem` getUserGroupUuidsForViewerPerm perms) currentUserGroupUuids)
        , or (fmap (`elem` getUserGroupUuidsForCommenterPerm perms) currentUserGroupUuids)
        , or (fmap (`elem` getUserGroupUuidsForEditorPerm perms) currentUserGroupUuids)
        , or (fmap (`elem` getUserGroupUuidsForOwnerPerm perms) currentUserGroupUuids)
        ]
        then return True
        else return False

checkCommentPermissionToProject :: ProjectPermC projectPerm => ProjectVisibility -> ProjectSharing -> [projectPerm] -> AppContextM ()
checkCommentPermissionToProject visibility sharing perms = do
  result <- hasCommentPermissionToProject visibility sharing perms
  if result
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Comment Project"

hasCommentPermissionToProject :: ProjectPermC projectPerm => ProjectVisibility -> ProjectSharing -> [projectPerm] -> AppContextM Bool
hasCommentPermissionToProject visibility sharing perms =
  if sharing == AnyoneWithLinkCommentProjectSharing || sharing == AnyoneWithLinkEditProjectSharing
    then return True
    else do
      checkPermission _PRJ_PERM
      currentUser <- getCurrentUser
      userGroupMemberships <- findUserGroupMembershipsByUserUuid currentUser.uuid
      let currentUserGroupUuids = fmap (.userGroupUuid) userGroupMemberships
      if or
        [ currentUser.uRole == _USER_ROLE_ADMIN
        , -- Check visibility
          visibility == VisibleCommentProjectVisibility
        , visibility == VisibleEditProjectVisibility
        , -- Check membership
          currentUser.uuid `elem` getUserUuidsForCommenterPerm perms
        , currentUser.uuid `elem` getUserUuidsForEditorPerm perms
        , currentUser.uuid `elem` getUserUuidsForOwnerPerm perms
        , -- Check groups
          or (fmap (`elem` getUserGroupUuidsForCommenterPerm perms) currentUserGroupUuids)
        , or (fmap (`elem` getUserGroupUuidsForEditorPerm perms) currentUserGroupUuids)
        , or (fmap (`elem` getUserGroupUuidsForOwnerPerm perms) currentUserGroupUuids)
        ]
        then return True
        else return False

checkEditPermissionToProject :: ProjectPermC projectPerm => ProjectVisibility -> ProjectSharing -> [projectPerm] -> AppContextM ()
checkEditPermissionToProject visibility sharing perms = do
  result <- hasEditPermissionToProject visibility sharing perms
  if result
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Edit Project"

hasEditPermissionToProject :: ProjectPermC projectPerm => ProjectVisibility -> ProjectSharing -> [projectPerm] -> AppContextM Bool
hasEditPermissionToProject visibility sharing perms =
  if sharing == AnyoneWithLinkEditProjectSharing
    then return True
    else do
      checkPermission _PRJ_PERM
      currentUser <- getCurrentUser
      userGroupMemberships <- findUserGroupMembershipsByUserUuid currentUser.uuid
      let currentUserGroupUuids = fmap (.userGroupUuid) userGroupMemberships
      if or
        [ currentUser.uRole == _USER_ROLE_ADMIN
        , -- Check visibility
          visibility == VisibleEditProjectVisibility
        , -- Check membership
          currentUser.uuid `elem` getUserUuidsForEditorPerm perms
        , currentUser.uuid `elem` getUserUuidsForOwnerPerm perms
        , -- Check groups
          or (fmap (`elem` getUserGroupUuidsForEditorPerm perms) currentUserGroupUuids)
        , or (fmap (`elem` getUserGroupUuidsForOwnerPerm perms) currentUserGroupUuids)
        ]
        then return True
        else return False

checkOwnerPermissionToProject :: ProjectPermC projectPerm => ProjectVisibility -> [projectPerm] -> AppContextM ()
checkOwnerPermissionToProject visibility perms = do
  result <- hasOwnerPermissionToProject visibility perms
  if result
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Administrate Project"

hasOwnerPermissionToProject :: ProjectPermC projectPerm => ProjectVisibility -> [projectPerm] -> AppContextM Bool
hasOwnerPermissionToProject visibility perms = do
  checkPermission _PRJ_PERM
  currentUser <- getCurrentUser
  userGroupMemberships <- findUserGroupMembershipsByUserUuid currentUser.uuid
  let currentUserGroupUuids = fmap (.userGroupUuid) userGroupMemberships
  if or
    [ currentUser.uRole == _USER_ROLE_ADMIN
    , -- Check membership
      currentUser.uuid `elem` getUserUuidsForOwnerPerm perms
    , -- Check groups
      or (fmap (`elem` getUserGroupUuidsForOwnerPerm perms) currentUserGroupUuids)
    ]
    then return True
    else return False

checkMigrationPermissionToProject :: ProjectPermC projectPerm => ProjectVisibility -> [projectPerm] -> AppContextM ()
checkMigrationPermissionToProject visibility perms = do
  result <- hasMigrationPermissionToProject visibility perms
  if result
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Migrate Project"

hasMigrationPermissionToProject :: ProjectPermC projectPerm => ProjectVisibility -> [projectPerm] -> AppContextM Bool
hasMigrationPermissionToProject visibility perms = do
  currentUser <- getCurrentUser
  userGroupMemberships <- findUserGroupMembershipsByUserUuid currentUser.uuid
  let currentUserGroupUuids = fmap (.userGroupUuid) userGroupMemberships
  if or
    [ currentUser.uRole == _USER_ROLE_ADMIN
    , -- Check visibility
      visibility == VisibleEditProjectVisibility
    , -- Check membership
      currentUser.uuid `elem` getUserUuidsForEditorPerm perms
    , currentUser.uuid `elem` getUserUuidsForOwnerPerm perms
    , -- Check groups
      or (fmap (`elem` getUserGroupUuidsForEditorPerm perms) currentUserGroupUuids)
    , or (fmap (`elem` getUserGroupUuidsForOwnerPerm perms) currentUserGroupUuids)
    ]
    then return True
    else return False
