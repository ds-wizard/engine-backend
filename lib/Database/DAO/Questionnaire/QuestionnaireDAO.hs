module Database.DAO.Questionnaire.QuestionnaireDAO where

import Control.Lens ((^.))
import Data.Bson
import Data.Bson.Generic
import Database.MongoDB
       ((=:), delete, deleteOne, fetch, find, findOne, insert, merge,
        rest, save, select)

import Database.BSON.Questionnaire.Questionnaire ()
import Database.DAO.Common
import LensesConfig
import Model.Context.AppContext
import Model.Error.Error
import Model.Questionnaire.Questionnaire

entityName = "questionnaire"

qtnCollection = "questionnaires"

findQuestionnaires :: AppContextM (Either AppError [Questionnaire])
findQuestionnaires = do
  let action = rest =<< find (select [] qtnCollection)
  questionnairesS <- runDB action
  return . deserializeEntities $ questionnairesS

findQuestionnaireByPackageId :: String -> AppContextM (Either AppError [Questionnaire])
findQuestionnaireByPackageId pkgId = do
  let action = rest =<< find (select ["packageId" =: pkgId] qtnCollection)
  questionnairesS <- runDB action
  return . deserializeEntities $ questionnairesS

findQuestionnaireById :: String -> AppContextM (Either AppError Questionnaire)
findQuestionnaireById uuid = do
  let action = findOne $ select ["uuid" =: uuid] qtnCollection
  maybeQuestionnaireS <- runDB action
  return . deserializeMaybeEntity entityName uuid $ maybeQuestionnaireS

findQuestionnaireByIdAndOwnerUuid :: String -> String -> AppContextM (Either AppError Questionnaire)
findQuestionnaireByIdAndOwnerUuid uuid ownerUuid = do
  let action = findOne $ select ["uuid" =: uuid, "ownerUuid" =: ownerUuid] qtnCollection
  maybeQuestionnaireS <- runDB action
  return . deserializeMaybeEntity entityName ("(uuid: " ++ uuid ++ ", ownerUuid: " ++ ownerUuid ++ ")") $
    maybeQuestionnaireS

insertQuestionnaire :: Questionnaire -> AppContextM Value
insertQuestionnaire questionnaire = do
  let action = insert qtnCollection (toBSON questionnaire)
  runDB action

updateQuestionnaireById :: Questionnaire -> AppContextM ()
updateQuestionnaireById questionnaire = do
  let action =
        fetch (select ["uuid" =: (questionnaire ^. uuid)] qtnCollection) >>=
        save qtnCollection . merge (toBSON questionnaire)
  runDB action

deleteQuestionnaires :: AppContextM ()
deleteQuestionnaires = do
  let action = delete $ select [] qtnCollection
  runDB action

deleteQuestionnaireById :: String -> AppContextM ()
deleteQuestionnaireById qtnUuid = do
  let action = deleteOne $ select ["uuid" =: qtnUuid] qtnCollection
  runDB action

-- --------------------------------
-- HELPERS
-- --------------------------------
heFindQuestionnaires callback = do
  eitherQuestionnaires <- findQuestionnaires
  case eitherQuestionnaires of
    Right questionnaires -> callback questionnaires
    Left error -> return . Left $ error

-- -----------------------------------------------------
heFindQuestionnaireById qtnUuid callback = do
  eitherQuestionnaire <- findQuestionnaireById qtnUuid
  case eitherQuestionnaire of
    Right questionnaire -> callback questionnaire
    Left error -> return . Left $ error

hmFindQuestionnaireById qtnUuid callback = do
  eitherQuestionnaire <- findQuestionnaireById qtnUuid
  case eitherQuestionnaire of
    Right questionnaire -> callback questionnaire
    Left error -> return . Just $ error

-- -----------------------------------------------------
heFindQuestionnaireByIdAndOwnerUuid qtnUuid ownerUuid callback = do
  eitherQuestionnaire <- findQuestionnaireByIdAndOwnerUuid qtnUuid ownerUuid
  case eitherQuestionnaire of
    Right questionnaire -> callback questionnaire
    Left error -> return . Left $ error

hmFindQuestionnaireByIdAndOwnerUuid qtnUuid ownerUuid callback = do
  eitherQuestionnaire <- findQuestionnaireByIdAndOwnerUuid qtnUuid ownerUuid
  case eitherQuestionnaire of
    Right questionnaire -> callback questionnaire
    Left error -> return . Just $ error
