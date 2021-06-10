module Wizard.Service.Questionnaire.QuestionnaireAcl where

import Control.Lens ((^.), (^..))
import Control.Monad (when)
import Control.Monad.Except (throwError)

import LensesConfig
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Localization.Messages.Public
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAcl
import Wizard.Model.Questionnaire.QuestionnaireAclHelpers
import Wizard.Model.User.User
import Wizard.Service.Acl.AclService
import Wizard.Service.Config.AppConfigService

checkCreatePermissionToQtn :: AppContextM ()
checkCreatePermissionToQtn = do
  appConfig <- getAppConfig
  let qtnSharingEnabled = appConfig ^. questionnaire . questionnaireSharing . enabled
  let qtnSharingAnonymousEnabled = appConfig ^. questionnaire . questionnaireSharing . anonymousEnabled
  let qtnCreation = appConfig ^. questionnaire . questionnaireCreation
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
  let qtnCreation = appConfig ^. questionnaire . questionnaireCreation
  case qtnCreation of
    CustomQuestionnaireCreation ->
      throwError . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Questionnaire Template"
    _ -> when (not isTemplate) (throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Questionnaire Template")

checkClonePermissionToQtn ::
     QuestionnaireVisibility -> QuestionnaireSharing -> [QuestionnairePermRecord] -> AppContextM ()
checkClonePermissionToQtn visibility sharing permissions = do
  checkPermission _QTN_PERM
  checkViewPermissionToQtn visibility sharing permissions

checkViewPermissionToQtn ::
     QuestionnaireVisibility -> QuestionnaireSharing -> [QuestionnairePermRecord] -> AppContextM ()
checkViewPermissionToQtn visibility sharing perms =
  if sharing == AnyoneWithLinkViewQuestionnaire || sharing == AnyoneWithLinkEditQuestionnaire
    then return ()
    else do
      checkPermission _QTN_PERM
      currentUser <- getCurrentUser
      let currentUserUuid = currentUser ^. uuid
      let currentUserGroupIds = currentUser ^. groups ^.. traverse . groupId
      if or
           [ currentUser ^. role == _USER_ROLE_ADMIN
           -- Check visibility
           , visibility == VisibleViewQuestionnaire
           , visibility == VisibleEditQuestionnaire
           -- Check membership
           , currentUserUuid `elem` getUserUuidsForViewerPerm perms
           , currentUserUuid `elem` getUserUuidsForEditorPerm perms
           , currentUserUuid `elem` getUserUuidsForOwnerPerm perms
           -- Check groups
           , or (fmap (`elem` getGroupIdsForViewerPerm perms) currentUserGroupIds)
           , or (fmap (`elem` getGroupIdsForEditorPerm perms) currentUserGroupIds)
           , or (fmap (`elem` getGroupIdsForOwnerPerm perms) currentUserGroupIds)
           ]
        then return ()
        else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "View Questionnaire"

checkEditPermissionToQtn ::
     QuestionnaireVisibility -> QuestionnaireSharing -> [QuestionnairePermRecord] -> AppContextM ()
checkEditPermissionToQtn visibility sharing perms =
  if sharing == AnyoneWithLinkEditQuestionnaire
    then return ()
    else do
      checkPermission _QTN_PERM
      currentUser <- getCurrentUser
      let currentUserUuid = currentUser ^. uuid
      let currentUserGroupIds = currentUser ^. groups ^.. traverse . groupId
      if or
           [ currentUser ^. role == _USER_ROLE_ADMIN
           -- Check visibility
           , visibility == VisibleEditQuestionnaire
           -- Check membership
           , currentUserUuid `elem` getUserUuidsForEditorPerm perms
           , currentUserUuid `elem` getUserUuidsForOwnerPerm perms
           -- Check groups
           , or (fmap (`elem` getGroupIdsForEditorPerm perms) currentUserGroupIds)
           , or (fmap (`elem` getGroupIdsForOwnerPerm perms) currentUserGroupIds)
           ]
        then return ()
        else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Edit Questionnaire"

checkOwnerPermissionToQtn :: QuestionnaireVisibility -> [QuestionnairePermRecord] -> AppContextM ()
checkOwnerPermissionToQtn visibility perms = do
  checkPermission _QTN_PERM
  currentUser <- getCurrentUser
  let currentUserUuid = currentUser ^. uuid
  let currentUserGroupIds = currentUser ^. groups ^.. traverse . groupId
  if or
       [ currentUser ^. role == _USER_ROLE_ADMIN
       -- Check membership
       , currentUserUuid `elem` getUserUuidsForOwnerPerm perms
       -- Check groups
       , or (fmap (`elem` getGroupIdsForOwnerPerm perms) currentUserGroupIds)
       ]
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Administrate Questionnaire"

checkMigrationPermissionToQtn :: QuestionnaireVisibility -> [QuestionnairePermRecord] -> AppContextM ()
checkMigrationPermissionToQtn visibility perms = do
  currentUser <- getCurrentUser
  let currentUserUuid = currentUser ^. uuid
  let currentUserGroupIds = currentUser ^. groups ^.. traverse . groupId
  if or
       [ currentUser ^. role == _USER_ROLE_ADMIN
       -- Check visibility
       , visibility == VisibleEditQuestionnaire
       -- Check membership
       , currentUserUuid `elem` getUserUuidsForEditorPerm perms
       , currentUserUuid `elem` getUserUuidsForOwnerPerm perms
       -- Check groups
       , or (fmap (`elem` getGroupIdsForEditorPerm perms) currentUserGroupIds)
       , or (fmap (`elem` getGroupIdsForOwnerPerm perms) currentUserGroupIds)
       ]
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Migrate Questionnaire"
