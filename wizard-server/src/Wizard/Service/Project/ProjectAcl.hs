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
    (_, _, TemplateProjectCreation) -> checkPermission _PROJECT_TEMPLATES_MANAGE_ROLE_PERMISSION
    (_, _, _) -> return ()

checkCreateFromTemplatePermissionToProject :: Bool -> AppContextM ()
checkCreateFromTemplatePermissionToProject isTemplate = do
  tcProject <- getCurrentTenantConfigProject
  let projectCreation = tcProject.projectCreation
  case projectCreation of
    CustomProjectCreation ->
      throwError . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Project Template"
    _ -> unless isTemplate (throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Project Template")

checkClonePermissionToProject :: ProjectPermC projectPerm => ProjectVisibility -> ProjectSharing -> [projectPerm] -> AppContextM ()
checkClonePermissionToProject visibility sharing permissions = do
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
      currentUser <- getCurrentUser
      userGroupMemberships <- findUserGroupMembershipsByUserUuid currentUser.uuid
      let currentUserGroupUuids = fmap (.userGroupUuid) userGroupMemberships
      hasPermission <- hasPermission _PROJECTS_VIEW_ROLE_PERMISSION
      if or
        [ hasPermission
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
          any (`elem` getUserGroupUuidsForViewerPerm perms) currentUserGroupUuids
        , any (`elem` getUserGroupUuidsForCommenterPerm perms) currentUserGroupUuids
        , any (`elem` getUserGroupUuidsForEditorPerm perms) currentUserGroupUuids
        , any (`elem` getUserGroupUuidsForOwnerPerm perms) currentUserGroupUuids
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
      currentUser <- getCurrentUser
      userGroupMemberships <- findUserGroupMembershipsByUserUuid currentUser.uuid
      let currentUserGroupUuids = fmap (.userGroupUuid) userGroupMemberships
      hasPermission <- hasPermission _PROJECTS_COMMENT_ROLE_PERMISSION
      if or
        [ hasPermission
        , -- Check visibility
          visibility == VisibleCommentProjectVisibility
        , visibility == VisibleEditProjectVisibility
        , -- Check membership
          currentUser.uuid `elem` getUserUuidsForCommenterPerm perms
        , currentUser.uuid `elem` getUserUuidsForEditorPerm perms
        , currentUser.uuid `elem` getUserUuidsForOwnerPerm perms
        , -- Check groups
          any (`elem` getUserGroupUuidsForCommenterPerm perms) currentUserGroupUuids
        , any (`elem` getUserGroupUuidsForEditorPerm perms) currentUserGroupUuids
        , any (`elem` getUserGroupUuidsForOwnerPerm perms) currentUserGroupUuids
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
      currentUser <- getCurrentUser
      userGroupMemberships <- findUserGroupMembershipsByUserUuid currentUser.uuid
      let currentUserGroupUuids = fmap (.userGroupUuid) userGroupMemberships
      hasPermission <- hasPermission _PROJECTS_EDIT_ROLE_PERMISSION
      if or
        [ hasPermission
        , -- Check visibility
          visibility == VisibleEditProjectVisibility
        , -- Check membership
          currentUser.uuid `elem` getUserUuidsForEditorPerm perms
        , currentUser.uuid `elem` getUserUuidsForOwnerPerm perms
        , -- Check groups
          any (`elem` getUserGroupUuidsForEditorPerm perms) currentUserGroupUuids
        , any (`elem` getUserGroupUuidsForOwnerPerm perms) currentUserGroupUuids
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
  currentUser <- getCurrentUser
  userGroupMemberships <- findUserGroupMembershipsByUserUuid currentUser.uuid
  let currentUserGroupUuids = fmap (.userGroupUuid) userGroupMemberships
  hasPermission <- hasPermission _PROJECTS_MANAGE_ROLE_PERMISSION
  if or
    [ hasPermission
    , -- Check membership
      currentUser.uuid `elem` getUserUuidsForOwnerPerm perms
    , -- Check groups
      any (`elem` getUserGroupUuidsForOwnerPerm perms) currentUserGroupUuids
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
  hasPermission <- hasPermission _PROJECTS_EDIT_ROLE_PERMISSION
  if or
    [ hasPermission
    , -- Check visibility
      visibility == VisibleEditProjectVisibility
    , -- Check membership
      currentUser.uuid `elem` getUserUuidsForEditorPerm perms
    , currentUser.uuid `elem` getUserUuidsForOwnerPerm perms
    , -- Check groups
      any (`elem` getUserGroupUuidsForEditorPerm perms) currentUserGroupUuids
    , any (`elem` getUserGroupUuidsForOwnerPerm perms) currentUserGroupUuids
    ]
    then return True
    else return False
