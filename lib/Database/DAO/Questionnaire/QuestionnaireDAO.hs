module Database.DAO.Questionnaire.QuestionnaireDAO where

import Control.Lens ((^.))
import Data.Bson
import Data.Bson.Generic
import Data.Time
import Database.MongoDB
       ((=:), delete, deleteOne, fetch, find, findOne, insert, merge,
        modify, rest, save, select)

import Database.BSON.Questionnaire.Questionnaire ()
import Database.DAO.Common
import LensesConfig
import Model.Context.AppContext
import Model.Error.Error
import Model.Questionnaire.Questionnaire

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
findQuestionnaireById questionnaireUuid = do
  let action = findOne $ select ["uuid" =: questionnaireUuid] qtnCollection
  maybeQuestionnaireS <- runDB action
  return . deserializeMaybeEntity $ maybeQuestionnaireS

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

updateQuestionnaireRepliesById :: String -> [QuestionnaireReply] -> UTCTime -> AppContextM ()
updateQuestionnaireRepliesById questionnaireUuid replies qtnUpdatedAt = do
  let action =
        modify
          (select ["uuid" =: questionnaireUuid] qtnCollection)
          ["$set" =: ["replies" =: replies, "updatedAt" =: qtnUpdatedAt]]
  runDB action

deleteQuestionnaires :: AppContextM ()
deleteQuestionnaires = do
  let action = delete $ select [] qtnCollection
  runDB action

deleteQuestionnaireById :: String -> AppContextM ()
deleteQuestionnaireById questionnaireUuid = do
  let action = deleteOne $ select ["uuid" =: questionnaireUuid] qtnCollection
  runDB action

-- --------------------------------
-- HELPERS
-- --------------------------------
heFindQuestionnaireById questionnaireUuid callback = do
  eitherQuestionnaire <- findQuestionnaireById questionnaireUuid
  case eitherQuestionnaire of
    Right questionnaire -> callback questionnaire
    Left error -> return . Left $ error
