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

checkPermissionToQtn :: QuestionnaireVisibility -> Maybe U.UUID -> AppContextM ()
checkPermissionToQtn visibility mOwnerUuid = do
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

checkMigrationPermissionToQtn :: QuestionnaireVisibility -> Maybe U.UUID -> AppContextM ()
checkMigrationPermissionToQtn visibility mOwnerUuid = do
  currentUser <- getCurrentUser
  if currentUser ^. role == _USER_ROLE_ADMIN ||
     visibility == PublicQuestionnaire || mOwnerUuid == Just (currentUser ^. uuid)
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Migrate Questionnaire"
