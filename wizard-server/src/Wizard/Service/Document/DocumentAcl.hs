module Wizard.Service.Document.DocumentAcl where

import qualified Data.UUID as U

import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Service.Questionnaire.QuestionnaireAcl

checkViewPermissionToDoc :: U.UUID -> AppContextM ()
checkViewPermissionToDoc qtnUuid = do
  qtn <- findQuestionnaireByUuid qtnUuid
  checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions

checkViewPermissionToDoc' :: Questionnaire -> AppContextM ()
checkViewPermissionToDoc' qtn = checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions

checkEditPermissionToDoc :: U.UUID -> AppContextM ()
checkEditPermissionToDoc qtnUuid = do
  _ <- getCurrentUser
  qtn <- findQuestionnaireByUuid qtnUuid
  checkEditPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
