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

checkPermissionToQtn :: QuestionnaireVisibility -> QuestionnaireSharing -> Maybe U.UUID -> AppContextM ()
checkPermissionToQtn visibility sharing mOwnerUuid =
  if sharing == AnyoneWithLinkQuestionnaire && visibility /= PrivateQuestionnaire
    then return ()
    else do
      checkPermission _QTN_PERM
      currentUser <- getCurrentUser
      if currentUser ^. role == _USER_ROLE_ADMIN ||
         visibility == PublicQuestionnaire ||
         visibility == PublicReadOnlyQuestionnaire || mOwnerUuid == Just (currentUser ^. uuid)
        then return ()
        else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Get Questionnaire"

checkEditPermissionToQtn :: QuestionnaireVisibility -> Maybe U.UUID -> AppContextM ()
checkEditPermissionToQtn visibility mOwnerUuid = do
  currentUser <- getCurrentUser
  if currentUser ^. role == _USER_ROLE_ADMIN ||
     visibility == PublicQuestionnaire || mOwnerUuid == Just (currentUser ^. uuid)
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Edit Questionnaire"

checkEditRepliesPermissionToQtn :: QuestionnaireVisibility -> QuestionnaireSharing -> Maybe U.UUID -> AppContextM ()
checkEditRepliesPermissionToQtn visibility sharing mOwnerUuid =
  if sharing == AnyoneWithLinkQuestionnaire && visibility == PublicQuestionnaire
    then return ()
    else do
      checkPermission _QTN_PERM
      currentUser <- getCurrentUser
      if currentUser ^. role == _USER_ROLE_ADMIN ||
         visibility == PublicQuestionnaire || mOwnerUuid == Just (currentUser ^. uuid)
        then return ()
        else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Edit Replies Questionnaire"

checkMigrationPermissionToQtn :: QuestionnaireVisibility -> Maybe U.UUID -> AppContextM ()
checkMigrationPermissionToQtn visibility mOwnerUuid = do
  currentUser <- getCurrentUser
  if currentUser ^. role == _USER_ROLE_ADMIN ||
     visibility == PublicQuestionnaire || mOwnerUuid == Just (currentUser ^. uuid)
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Migrate Questionnaire"
