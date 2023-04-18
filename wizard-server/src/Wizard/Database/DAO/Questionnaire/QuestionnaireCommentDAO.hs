module Wizard.Database.DAO.Questionnaire.QuestionnaireCommentDAO where

import Control.Monad.Reader (liftIO)
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Util.String
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Questionnaire.QuestionnaireComment ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireCommentThread ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Questionnaire.QuestionnaireComment

entityName = "questionnaire_comment"

findQuestionnaireComments :: AppContextM [QuestionnaireComment]
findQuestionnaireComments = createFindEntitiesFn entityName

insertQuestionnaireComment :: QuestionnaireComment -> AppContextM Int64
insertQuestionnaireComment = createInsertFn entityName

insertQuestionnaireThreadAndComment :: QuestionnaireCommentThread -> QuestionnaireComment -> AppContextM Int64
insertQuestionnaireThreadAndComment thread comment = do
  let sql =
        fromString $
          f'
            "BEGIN TRANSACTION; \
            \INSERT INTO %s VALUES (%s); \
            \INSERT INTO %s VALUES (%s); \
            \COMMIT;"
            ["questionnaire_comment_thread", generateQuestionMarks' thread, entityName, generateQuestionMarks' comment]
  let params = toRow thread ++ toRow comment
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action

insertQuestionnaireThreadAndComment' :: QuestionnaireCommentThread -> QuestionnaireComment -> AppContextM Int64
insertQuestionnaireThreadAndComment' thread comment = do
  let sql =
        fromString $
          f'
            "INSERT INTO %s VALUES (%s); \
            \INSERT INTO %s VALUES (%s); "
            ["questionnaire_comment_thread", generateQuestionMarks' thread, entityName, generateQuestionMarks' comment]
  let params = toRow thread ++ toRow comment
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action

updateQuestionnaireCommentById :: QuestionnaireComment -> AppContextM QuestionnaireComment
updateQuestionnaireCommentById entity = do
  now <- liftIO getCurrentTime
  let updatedEntity = entity {updatedAt = now} :: QuestionnaireComment
  let sql =
        fromString
          "UPDATE questionnaire_comment SET uuid = ?, text = ?, created_by = ?, created_at = ?, updated_at = ? WHERE uuid = ?"
  let params = toRow updatedEntity ++ [toField updatedEntity.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  return updatedEntity

updateQuestionnaireCommentTextById :: U.UUID -> String -> AppContextM Int64
updateQuestionnaireCommentTextById uuid text = do
  let sql = fromString "UPDATE questionnaire_comment SET text = ?, updated_at = now() WHERE uuid = ?"
  let params = [toField text, toField uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteQuestionnaireComments :: AppContextM Int64
deleteQuestionnaireComments = createDeleteEntitiesFn entityName

deleteQuestionnaireCommentsByThreadUuid :: U.UUID -> AppContextM Int64
deleteQuestionnaireCommentsByThreadUuid threadUuid =
  createDeleteEntityByFn entityName [("comment_thread_uuid", U.toString threadUuid)]

deleteQuestionnaireCommentById :: U.UUID -> AppContextM Int64
deleteQuestionnaireCommentById uuid = createDeleteEntityByFn entityName [("uuid", U.toString uuid)]
