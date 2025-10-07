module Wizard.Service.Questionnaire.QuestionnaireAcl where

import Control.Monad (unless)
import Control.Monad.Except (throwError)

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAclHelpers
import Wizard.Model.Questionnaire.QuestionnairePerm
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.Tenant.Config.ConfigService
import WizardLib.Public.Database.DAO.User.UserGroupMembershipDAO
import WizardLib.Public.Model.User.UserGroupMembership

checkCreatePermissionToQtn :: AppContextM ()
checkCreatePermissionToQtn = do
  tcQuestionnaire <- getCurrentTenantConfigQuestionnaire
  let qtnSharingEnabled = tcQuestionnaire.questionnaireSharing.enabled
  let qtnSharingAnonymousEnabled = tcQuestionnaire.questionnaireSharing.anonymousEnabled
  let qtnCreation = tcQuestionnaire.questionnaireCreation
  case (qtnSharingEnabled, qtnSharingAnonymousEnabled, qtnCreation) of
    (True, True, CustomQuestionnaireCreation) -> return ()
    (True, True, TemplateAndCustomQuestionnaireCreation) -> return ()
    (_, _, TemplateQuestionnaireCreation) -> do
      checkPermission _QTN_PERM
      checkPermission _QTN_TML_PERM
    (_, _, _) -> checkPermission _QTN_PERM

checkCreateFromTemplatePermissionToQtn :: Bool -> AppContextM ()
checkCreateFromTemplatePermissionToQtn isTemplate = do
  checkPermission _QTN_PERM
  tcQuestionnaire <- getCurrentTenantConfigQuestionnaire
  let qtnCreation = tcQuestionnaire.questionnaireCreation
  case qtnCreation of
    CustomQuestionnaireCreation ->
      throwError . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Questionnaire Template"
    _ -> unless isTemplate (throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Questionnaire Template")

checkClonePermissionToQtn :: QuestionnairePermC questionnairePerm => QuestionnaireVisibility -> QuestionnaireSharing -> [questionnairePerm] -> AppContextM ()
checkClonePermissionToQtn visibility sharing permissions = do
  checkPermission _QTN_PERM
  checkViewPermissionToQtn visibility sharing permissions

checkViewPermissionToQtn :: QuestionnairePermC questionnairePerm => QuestionnaireVisibility -> QuestionnaireSharing -> [questionnairePerm] -> AppContextM ()
checkViewPermissionToQtn visibility sharing perms = do
  result <- hasViewPermissionToQtn visibility sharing perms
  if result
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "View Questionnaire"

hasViewPermissionToQtn :: QuestionnairePermC questionnairePerm => QuestionnaireVisibility -> QuestionnaireSharing -> [questionnairePerm] -> AppContextM Bool
hasViewPermissionToQtn visibility sharing perms =
  if sharing == AnyoneWithLinkViewQuestionnaire
    || sharing == AnyoneWithLinkCommentQuestionnaire
    || sharing
      == AnyoneWithLinkEditQuestionnaire
    then return True
    else do
      checkPermission _QTN_PERM
      currentUser <- getCurrentUser
      userGroupMemberships <- findUserGroupMembershipsByUserUuid currentUser.uuid
      let currentUserGroupUuids = fmap (.userGroupUuid) userGroupMemberships
      if or
        [ currentUser.uRole == _USER_ROLE_ADMIN
        , -- Check visibility
          visibility == VisibleViewQuestionnaire
        , visibility == VisibleCommentQuestionnaire
        , visibility == VisibleEditQuestionnaire
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

checkCommentPermissionToQtn :: QuestionnairePermC questionnairePerm => QuestionnaireVisibility -> QuestionnaireSharing -> [questionnairePerm] -> AppContextM ()
checkCommentPermissionToQtn visibility sharing perms = do
  result <- hasCommentPermissionToQtn visibility sharing perms
  if result
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Comment Questionnaire"

hasCommentPermissionToQtn :: QuestionnairePermC questionnairePerm => QuestionnaireVisibility -> QuestionnaireSharing -> [questionnairePerm] -> AppContextM Bool
hasCommentPermissionToQtn visibility sharing perms =
  if sharing == AnyoneWithLinkCommentQuestionnaire || sharing == AnyoneWithLinkEditQuestionnaire
    then return True
    else do
      checkPermission _QTN_PERM
      currentUser <- getCurrentUser
      userGroupMemberships <- findUserGroupMembershipsByUserUuid currentUser.uuid
      let currentUserGroupUuids = fmap (.userGroupUuid) userGroupMemberships
      if or
        [ currentUser.uRole == _USER_ROLE_ADMIN
        , -- Check visibility
          visibility == VisibleCommentQuestionnaire
        , visibility == VisibleEditQuestionnaire
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

checkEditPermissionToQtn :: QuestionnairePermC questionnairePerm => QuestionnaireVisibility -> QuestionnaireSharing -> [questionnairePerm] -> AppContextM ()
checkEditPermissionToQtn visibility sharing perms = do
  result <- hasEditPermissionToQtn visibility sharing perms
  if result
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Edit Questionnaire"

hasEditPermissionToQtn :: QuestionnairePermC questionnairePerm => QuestionnaireVisibility -> QuestionnaireSharing -> [questionnairePerm] -> AppContextM Bool
hasEditPermissionToQtn visibility sharing perms =
  if sharing == AnyoneWithLinkEditQuestionnaire
    then return True
    else do
      checkPermission _QTN_PERM
      currentUser <- getCurrentUser
      userGroupMemberships <- findUserGroupMembershipsByUserUuid currentUser.uuid
      let currentUserGroupUuids = fmap (.userGroupUuid) userGroupMemberships
      if or
        [ currentUser.uRole == _USER_ROLE_ADMIN
        , -- Check visibility
          visibility == VisibleEditQuestionnaire
        , -- Check membership
          currentUser.uuid `elem` getUserUuidsForEditorPerm perms
        , currentUser.uuid `elem` getUserUuidsForOwnerPerm perms
        , -- Check groups
          or (fmap (`elem` getUserGroupUuidsForEditorPerm perms) currentUserGroupUuids)
        , or (fmap (`elem` getUserGroupUuidsForOwnerPerm perms) currentUserGroupUuids)
        ]
        then return True
        else return False

checkOwnerPermissionToQtn :: QuestionnairePermC questionnairePerm => QuestionnaireVisibility -> [questionnairePerm] -> AppContextM ()
checkOwnerPermissionToQtn visibility perms = do
  result <- hasOwnerPermissionToQtn visibility perms
  if result
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Administrate Questionnaire"

hasOwnerPermissionToQtn :: QuestionnairePermC questionnairePerm => QuestionnaireVisibility -> [questionnairePerm] -> AppContextM Bool
hasOwnerPermissionToQtn visibility perms = do
  checkPermission _QTN_PERM
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

checkMigrationPermissionToQtn :: QuestionnairePermC questionnairePerm => QuestionnaireVisibility -> [questionnairePerm] -> AppContextM ()
checkMigrationPermissionToQtn visibility perms = do
  result <- hasMigrationPermissionToQtn visibility perms
  if result
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Migrate Questionnaire"

hasMigrationPermissionToQtn :: QuestionnairePermC questionnairePerm => QuestionnaireVisibility -> [questionnairePerm] -> AppContextM Bool
hasMigrationPermissionToQtn visibility perms = do
  currentUser <- getCurrentUser
  userGroupMemberships <- findUserGroupMembershipsByUserUuid currentUser.uuid
  let currentUserGroupUuids = fmap (.userGroupUuid) userGroupMemberships
  if or
    [ currentUser.uRole == _USER_ROLE_ADMIN
    , -- Check visibility
      visibility == VisibleEditQuestionnaire
    , -- Check membership
      currentUser.uuid `elem` getUserUuidsForEditorPerm perms
    , currentUser.uuid `elem` getUserUuidsForOwnerPerm perms
    , -- Check groups
      or (fmap (`elem` getUserGroupUuidsForEditorPerm perms) currentUserGroupUuids)
    , or (fmap (`elem` getUserGroupUuidsForOwnerPerm perms) currentUserGroupUuids)
    ]
    then return True
    else return False
