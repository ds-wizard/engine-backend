module Database.DAO.Questionnaire.QuestionnaireDAO where

import Control.Lens ((^.))
import Data.Bson

import Database.BSON.Questionnaire.Questionnaire ()
import Database.DAO.Common
import LensesConfig
import Model.Context.AppContext
import Model.Error.Error
import Model.Questionnaire.Questionnaire
import Util.Helper (createHeeHelper)

entityName = "questionnaire"

collection = "questionnaires"

findQuestionnaires :: AppContextM (Either AppError [Questionnaire])
findQuestionnaires = createFindEntitiesFn collection

findQuestionnaireByPackageId :: String -> AppContextM (Either AppError [Questionnaire])
findQuestionnaireByPackageId packageId = createFindEntitiesByFn collection ["packageId" =: packageId]

findQuestionnaireById :: String -> AppContextM (Either AppError Questionnaire)
findQuestionnaireById = createFindEntityByFn collection entityName "uuid"

countQuestionnaires :: AppContextM (Either AppError Int)
countQuestionnaires = createCountFn collection

insertQuestionnaire :: Questionnaire -> AppContextM Value
insertQuestionnaire = createInsertFn collection

updateQuestionnaireById :: Questionnaire -> AppContextM ()
updateQuestionnaireById qtn = createUpdateByFn collection "uuid" (qtn ^. uuid) qtn

deleteQuestionnaires :: AppContextM ()
deleteQuestionnaires = createDeleteEntitiesFn collection

deleteQuestionnaireById :: String -> AppContextM ()
deleteQuestionnaireById = createDeleteEntityByFn collection "uuid"

-- --------------------------------
-- HELPERS
-- --------------------------------
heFindQuestionnaires callback = createHeeHelper findQuestionnaires callback

-- -----------------------------------------------------
heFindQuestionnaireById qtnUuid callback = createHeeHelper (findQuestionnaireById qtnUuid) callback

-- -----------------------------------------------------
heCountQuestionnaires callback = createHeeHelper countQuestionnaires callback
