module Wizard.Service.Questionnaire.File.QuestionnaireFileAcl where

import qualified Data.UUID as U

import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Service.Questionnaire.QuestionnaireAcl

checkViewPermissionToFile :: U.UUID -> AppContextM ()
checkViewPermissionToFile qtnUuid = do
  qtn <- findQuestionnaireByUuid qtnUuid
  checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions

checkEditPermissionToFile :: U.UUID -> AppContextM ()
checkEditPermissionToFile qtnUuid = do
  qtn <- findQuestionnaireByUuid qtnUuid
  checkEditPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
