module Wizard.Service.Document.DocumentACL where

import Control.Lens ((^.))

import LensesConfig
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.Questionnaire.QuestionnaireACL

checkViewPermissionToDoc :: String -> AppContextM ()
checkViewPermissionToDoc qtnUuid = do
  qtn <- findQuestionnaireById qtnUuid
  checkViewPermissionToQtn (qtn ^. visibility) (qtn ^. sharing) (qtn ^. ownerUuid)

checkEditPermissionToDoc :: String -> AppContextM ()
checkEditPermissionToDoc qtnUuid = do
  qtn <- findQuestionnaireById qtnUuid
  checkEditContentPermissionToQtn (qtn ^. visibility) (qtn ^. sharing) (qtn ^. ownerUuid)
