module Wizard.Service.Document.DocumentAcl where

import Control.Monad.Except (throwError)
import qualified Data.UUID as U

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Service.Questionnaire.QuestionnaireAcl

checkViewPermissionToDoc :: Maybe U.UUID -> AppContextM ()
checkViewPermissionToDoc mQtnUuid = do
  case mQtnUuid of
    Just qtnUuid -> do
      qtn <- findQuestionnaireByUuid qtnUuid
      checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
    Nothing -> throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Read Document"

checkViewPermissionToDoc' :: Questionnaire -> AppContextM ()
checkViewPermissionToDoc' qtn = checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions

checkEditPermissionToDoc :: Maybe U.UUID -> AppContextM ()
checkEditPermissionToDoc mQtnUuid = do
  case mQtnUuid of
    Just qtnUuid -> do
      _ <- getCurrentUser
      qtn <- findQuestionnaireByUuid qtnUuid
      checkEditPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
    Nothing -> throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Edit Document"
