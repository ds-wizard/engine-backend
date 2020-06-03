module Wizard.Database.DAO.Questionnaire.QuestionnaireDAO where

import Control.Lens ((^.))
import Data.Bson

import LensesConfig
import Shared.Database.DAO.Common
import Wizard.Database.BSON.Questionnaire.Questionnaire ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextLenses ()
import Wizard.Model.Questionnaire.Questionnaire

entityName = "questionnaire"

collection = "questionnaires"

findQuestionnaires :: AppContextM [Questionnaire]
findQuestionnaires = createFindEntitiesFn collection

findQuestionnaireByPackageId :: String -> AppContextM [Questionnaire]
findQuestionnaireByPackageId packageId = createFindEntitiesByFn collection ["packageId" =: packageId]

findQuestionnaireById :: String -> AppContextM Questionnaire
findQuestionnaireById = createFindEntityByFn collection entityName "uuid"

findQuestionnaireById' :: String -> AppContextM (Maybe Questionnaire)
findQuestionnaireById' = createFindEntityByFn' collection entityName "uuid"

countQuestionnaires :: AppContextM Int
countQuestionnaires = createCountFn collection

insertQuestionnaire :: Questionnaire -> AppContextM Value
insertQuestionnaire = createInsertFn collection

updateQuestionnaireById :: Questionnaire -> AppContextM ()
updateQuestionnaireById qtn = createUpdateByFn collection "uuid" (qtn ^. uuid) qtn

deleteQuestionnaires :: AppContextM ()
deleteQuestionnaires = createDeleteEntitiesFn collection

deleteQuestionnairesFiltered :: [(String, String)] -> AppContextM ()
deleteQuestionnairesFiltered queryParams = createDeleteEntitiesByFn collection (mapToDBQueryParams queryParams)

deleteQuestionnaireById :: String -> AppContextM ()
deleteQuestionnaireById = createDeleteEntityByFn collection "uuid"
