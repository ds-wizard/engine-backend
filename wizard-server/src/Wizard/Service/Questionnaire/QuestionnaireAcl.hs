module Wizard.Service.Questionnaire.QuestionnaireAcl where

import Control.Monad (unless)
import Control.Monad.Except (throwError)

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.User.UserDTO
import Wizard.Localization.Messages.Public
import Wizard.Model.Acl.Acl
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAcl
import Wizard.Model.Questionnaire.QuestionnaireAclHelpers
import Wizard.Model.User.User
import Wizard.Service.Acl.AclService
import Wizard.Service.Config.App.AppConfigService

checkCreatePermissionToQtn :: AppContextM ()
checkCreatePermissionToQtn = do
  appConfig <- getAppConfig
  let qtnSharingEnabled = appConfig.questionnaire.questionnaireSharing.enabled
  let qtnSharingAnonymousEnabled = appConfig.questionnaire.questionnaireSharing.anonymousEnabled
  let qtnCreation = appConfig.questionnaire.questionnaireCreation
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
  appConfig <- getAppConfig
  let qtnCreation = appConfig.questionnaire.questionnaireCreation
  case qtnCreation of
    CustomQuestionnaireCreation ->
      throwError . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Questionnaire Template"
    _ -> unless isTemplate (throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Questionnaire Template")

checkClonePermissionToQtn
  :: QuestionnaireVisibility -> QuestionnaireSharing -> [QuestionnairePermRecord] -> AppContextM ()
checkClonePermissionToQtn visibility sharing permissions = do
  checkPermission _QTN_PERM
  checkViewPermissionToQtn visibility sharing permissions

checkViewPermissionToQtn
  :: QuestionnaireVisibility -> QuestionnaireSharing -> [QuestionnairePermRecord] -> AppContextM ()
checkViewPermissionToQtn visibility sharing perms = do
  result <- hasViewPermissionToQtn visibility sharing perms
  if result
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "View Questionnaire"

hasViewPermissionToQtn
  :: QuestionnaireVisibility -> QuestionnaireSharing -> [QuestionnairePermRecord] -> AppContextM Bool
hasViewPermissionToQtn visibility sharing perms =
  if sharing == AnyoneWithLinkViewQuestionnaire
    || sharing == AnyoneWithLinkCommentQuestionnaire
    || sharing
      == AnyoneWithLinkEditQuestionnaire
    then return True
    else do
      checkPermission _QTN_PERM
      currentUser <- getCurrentUser
      let currentUserUuid = currentUser.uuid
      let currentUserGroupIds = fmap (.groupId) $ currentUser.groups
      if or
        [ currentUser.uRole == _USER_ROLE_ADMIN
        , -- Check visibility
          visibility == VisibleViewQuestionnaire
        , visibility == VisibleCommentQuestionnaire
        , visibility == VisibleEditQuestionnaire
        , -- Check membership
          currentUserUuid `elem` getUserUuidsForViewerPerm perms
        , currentUserUuid `elem` getUserUuidsForCommentatorPerm perms
        , currentUserUuid `elem` getUserUuidsForEditorPerm perms
        , currentUserUuid `elem` getUserUuidsForOwnerPerm perms
        , -- Check groups
          or (fmap (`elem` getGroupIdsForViewerPerm perms) currentUserGroupIds)
        , or (fmap (`elem` getGroupIdsForCommentatorPerm perms) currentUserGroupIds)
        , or (fmap (`elem` getGroupIdsForEditorPerm perms) currentUserGroupIds)
        , or (fmap (`elem` getGroupIdsForOwnerPerm perms) currentUserGroupIds)
        ]
        then return True
        else return False

checkCommentPermissionToQtn
  :: QuestionnaireVisibility -> QuestionnaireSharing -> [QuestionnairePermRecord] -> AppContextM ()
checkCommentPermissionToQtn visibility sharing perms = do
  result <- hasCommentPermissionToQtn visibility sharing perms
  if result
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Comment Questionnaire"

hasCommentPermissionToQtn
  :: QuestionnaireVisibility -> QuestionnaireSharing -> [QuestionnairePermRecord] -> AppContextM Bool
hasCommentPermissionToQtn visibility sharing perms =
  if sharing == AnyoneWithLinkCommentQuestionnaire || sharing == AnyoneWithLinkEditQuestionnaire
    then return True
    else do
      checkPermission _QTN_PERM
      currentUser <- getCurrentUser
      let currentUserUuid = currentUser.uuid
      let currentUserGroupIds = fmap (.groupId) $ currentUser.groups
      if or
        [ currentUser.uRole == _USER_ROLE_ADMIN
        , -- Check visibility
          visibility == VisibleCommentQuestionnaire
        , visibility == VisibleEditQuestionnaire
        , -- Check membership
          currentUserUuid `elem` getUserUuidsForCommentatorPerm perms
        , currentUserUuid `elem` getUserUuidsForEditorPerm perms
        , currentUserUuid `elem` getUserUuidsForOwnerPerm perms
        , -- Check groups
          or (fmap (`elem` getGroupIdsForCommentatorPerm perms) currentUserGroupIds)
        , or (fmap (`elem` getGroupIdsForEditorPerm perms) currentUserGroupIds)
        , or (fmap (`elem` getGroupIdsForOwnerPerm perms) currentUserGroupIds)
        ]
        then return True
        else return False

checkEditPermissionToQtn
  :: QuestionnaireVisibility -> QuestionnaireSharing -> [QuestionnairePermRecord] -> AppContextM ()
checkEditPermissionToQtn visibility sharing perms = do
  result <- hasEditPermissionToQtn visibility sharing perms
  if result
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Edit Questionnaire"

hasEditPermissionToQtn
  :: QuestionnaireVisibility -> QuestionnaireSharing -> [QuestionnairePermRecord] -> AppContextM Bool
hasEditPermissionToQtn visibility sharing perms =
  if sharing == AnyoneWithLinkEditQuestionnaire
    then return True
    else do
      checkPermission _QTN_PERM
      currentUser <- getCurrentUser
      let currentUserUuid = currentUser.uuid
      let currentUserGroupIds = fmap (.groupId) $ currentUser.groups
      if or
        [ currentUser.uRole == _USER_ROLE_ADMIN
        , -- Check visibility
          visibility == VisibleEditQuestionnaire
        , -- Check membership
          currentUserUuid `elem` getUserUuidsForEditorPerm perms
        , currentUserUuid `elem` getUserUuidsForOwnerPerm perms
        , -- Check groups
          or (fmap (`elem` getGroupIdsForEditorPerm perms) currentUserGroupIds)
        , or (fmap (`elem` getGroupIdsForOwnerPerm perms) currentUserGroupIds)
        ]
        then return True
        else return False

checkOwnerPermissionToQtn :: QuestionnaireVisibility -> [QuestionnairePermRecord] -> AppContextM ()
checkOwnerPermissionToQtn visibility perms = do
  result <- hasOwnerPermissionToQtn visibility perms
  if result
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Administrate Questionnaire"

hasOwnerPermissionToQtn :: QuestionnaireVisibility -> [QuestionnairePermRecord] -> AppContextM Bool
hasOwnerPermissionToQtn visibility perms = do
  checkPermission _QTN_PERM
  currentUser <- getCurrentUser
  let currentUserUuid = currentUser.uuid
  let currentUserGroupIds = fmap (.groupId) $ currentUser.groups
  if or
    [ currentUser.uRole == _USER_ROLE_ADMIN
    , -- Check membership
      currentUserUuid `elem` getUserUuidsForOwnerPerm perms
    , -- Check groups
      or (fmap (`elem` getGroupIdsForOwnerPerm perms) currentUserGroupIds)
    ]
    then return True
    else return False

checkMigrationPermissionToQtn :: QuestionnaireVisibility -> [QuestionnairePermRecord] -> AppContextM ()
checkMigrationPermissionToQtn visibility perms = do
  result <- hasMigrationPermissionToQtn visibility perms
  if result
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Migrate Questionnaire"

hasMigrationPermissionToQtn :: QuestionnaireVisibility -> [QuestionnairePermRecord] -> AppContextM Bool
hasMigrationPermissionToQtn visibility perms = do
  currentUser <- getCurrentUser
  let currentUserUuid = currentUser.uuid
  let currentUserGroupIds = fmap (.groupId) $ currentUser.groups
  if or
    [ currentUser.uRole == _USER_ROLE_ADMIN
    , -- Check visibility
      visibility == VisibleEditQuestionnaire
    , -- Check membership
      currentUserUuid `elem` getUserUuidsForEditorPerm perms
    , currentUserUuid `elem` getUserUuidsForOwnerPerm perms
    , -- Check groups
      or (fmap (`elem` getGroupIdsForEditorPerm perms) currentUserGroupIds)
    , or (fmap (`elem` getGroupIdsForOwnerPerm perms) currentUserGroupIds)
    ]
    then return True
    else return False
