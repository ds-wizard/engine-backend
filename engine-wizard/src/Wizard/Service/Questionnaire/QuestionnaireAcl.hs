module Wizard.Service.Questionnaire.QuestionnaireAcl where

import Control.Lens ((^.), (^..))
import Control.Monad.Except (throwError)

import LensesConfig
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAcl
import Wizard.Model.Questionnaire.QuestionnaireAclHelpers
import Wizard.Model.User.User
import Wizard.Service.Acl.AclService

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
