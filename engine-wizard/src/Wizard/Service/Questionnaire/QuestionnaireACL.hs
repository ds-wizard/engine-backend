module Wizard.Service.Questionnaire.QuestionnaireACL where

import Control.Lens ((^.))
import Control.Monad.Except (throwError)
import qualified Data.UUID as U

import LensesConfig
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.User.User
import Wizard.Service.Common.ACL

checkViewPermissionToQtn :: QuestionnaireVisibility -> QuestionnaireSharing -> Maybe U.UUID -> AppContextM ()
checkViewPermissionToQtn visibility sharing mOwnerUuid =
  if sharing == AnyoneWithLinkViewQuestionnaire || sharing == AnyoneWithLinkEditQuestionnaire
    then return ()
    else do
      checkPermission _QTN_PERM
      currentUser <- getCurrentUser
      if currentUser ^. role == _USER_ROLE_ADMIN ||
         visibility == VisibleEditQuestionnaire ||
         visibility == VisibleViewQuestionnaire || mOwnerUuid == Just (currentUser ^. uuid)
        then return ()
        else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Get Questionnaire"

checkEditPermissionToQtn :: QuestionnaireVisibility -> Maybe U.UUID -> AppContextM ()
checkEditPermissionToQtn visibility mOwnerUuid = do
  checkPermission _QTN_PERM
  currentUser <- getCurrentUser
  if currentUser ^. role == _USER_ROLE_ADMIN ||
     visibility == VisibleEditQuestionnaire || mOwnerUuid == Just (currentUser ^. uuid)
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Edit Questionnaire"

checkEditContentPermissionToQtn :: QuestionnaireVisibility -> QuestionnaireSharing -> Maybe U.UUID -> AppContextM ()
checkEditContentPermissionToQtn visibility sharing mOwnerUuid =
  if sharing == AnyoneWithLinkEditQuestionnaire
    then return ()
    else do
      checkPermission _QTN_PERM
      currentUser <- getCurrentUser
      if currentUser ^. role == _USER_ROLE_ADMIN ||
         visibility == VisibleEditQuestionnaire || mOwnerUuid == Just (currentUser ^. uuid)
        then return ()
        else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Edit Replies Questionnaire"

checkMigrationPermissionToQtn :: QuestionnaireVisibility -> Maybe U.UUID -> AppContextM ()
checkMigrationPermissionToQtn visibility mOwnerUuid = do
  currentUser <- getCurrentUser
  if currentUser ^. role == _USER_ROLE_ADMIN ||
     visibility == VisibleEditQuestionnaire || mOwnerUuid == Just (currentUser ^. uuid)
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Migrate Questionnaire"
