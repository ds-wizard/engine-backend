module Database.DAO.Questionnaire.QuestionnaireDAO where

import Control.Lens ((^.))
import Data.Bson
import Data.Bson.Generic
import Data.Map (Map)
import Data.Maybe
import Database.MongoDB
       ((=:), delete, deleteOne, fetch, find, findOne, insert, merge,
        modify, rest, save, select)
import Database.Persist.MongoDB (runMongoDBPoolDef)

import Common.Context
import Common.Error
import Common.Types
import Database.BSON.Questionnaire.Questionnaire
import Database.DAO.Common
import LensesConfig
import Model.Questionnaire.Questionnaire

qtnCollection = "questionnaires"

findQuestionnaires :: Context -> IO (Either AppError [Questionnaire])
findQuestionnaires context = do
  let action = rest =<< find (select [] qtnCollection)
  questionnairesS <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return . deserializeEntities $ questionnairesS

findQuestionnaireById :: Context -> String -> IO (Either AppError Questionnaire)
findQuestionnaireById context questionnaireUuid = do
  let action = findOne $ select ["uuid" =: questionnaireUuid] qtnCollection
  maybeQuestionnaireS <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return . deserializeMaybeEntity $ maybeQuestionnaireS

insertQuestionnaire :: Context -> Questionnaire -> IO Value
insertQuestionnaire context questionnaire = do
  let action = insert qtnCollection (toBSON questionnaire)
  runMongoDBPoolDef action (context ^. ctxDbPool)

updateQuestionnaireById :: Context -> Questionnaire -> IO ()
updateQuestionnaireById context questionnaire = do
  let action =
        fetch (select ["uuid" =: (questionnaire ^. uuid)] qtnCollection) >>=
        save qtnCollection . merge (toBSON questionnaire)
  runMongoDBPoolDef action (context ^. ctxDbPool)

updateQuestionnaireRepliesById :: Context -> String -> Map String String -> IO ()
updateQuestionnaireRepliesById context questionnaireUuid replies = do
  let action = modify (select ["uuid" =: questionnaireUuid] qtnCollection) ["$set" =: ["replies" =: replies]]
  runMongoDBPoolDef action (context ^. ctxDbPool)

deleteQuestionnaires :: Context -> IO ()
deleteQuestionnaires context = do
  let action = delete $ select [] qtnCollection
  runMongoDBPoolDef action (context ^. ctxDbPool)

deleteQuestionnaireById :: Context -> String -> IO ()
deleteQuestionnaireById context questionnaireUuid = do
  let action = deleteOne $ select ["uuid" =: questionnaireUuid] qtnCollection
  runMongoDBPoolDef action (context ^. ctxDbPool)
