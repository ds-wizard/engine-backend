module Database.DAO.PublicQuestionnaire.PublicQuestionnaireDAO where

import Data.Bson
import Data.Bson.Generic
import Database.MongoDB
       (delete, fetch, findOne, insert, merge, save, select)

import Database.BSON.Questionnaire.Questionnaire ()
import Database.DAO.Common
import Model.Context.AppContext
import Model.Error.Error
import Model.Questionnaire.Questionnaire

pubQtnCollection = "publicQuestionnaires"

findPublicQuestionnaire :: AppContextM (Either AppError Questionnaire)
findPublicQuestionnaire = do
  let action = findOne $ select [] pubQtnCollection
  maybePublicQuestionnaire <- runDB action
  return . deserializeMaybeEntity $ maybePublicQuestionnaire

insertPublicQuestionnaire :: Questionnaire -> AppContextM Value
insertPublicQuestionnaire pubQtn = do
  let action = insert pubQtnCollection (toBSON pubQtn)
  runDB action

updatePublicQuestionnaire :: Questionnaire -> AppContextM ()
updatePublicQuestionnaire pubQtn = do
  let action = fetch (select [] pubQtnCollection) >>= save pubQtnCollection . merge (toBSON pubQtn)
  runDB action

deletePublicQuestionnaires :: AppContextM ()
deletePublicQuestionnaires = do
  let action = delete $ select [] pubQtnCollection
  runDB action

-- --------------------------------
-- HELPERS
-- --------------------------------
heFindPublicQuestionnaire callback = do
  eitherQuestionnaire <- findPublicQuestionnaire
  case eitherQuestionnaire of
    Right questionnaire -> callback questionnaire
    Left error -> return . Left $ error
