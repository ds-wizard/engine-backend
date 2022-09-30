module Wizard.Database.DAO.Questionnaire.QuestionnaireCommentThreadDAO where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import LensesConfig
import Shared.Model.Error.Error
import Shared.Util.String
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Questionnaire.QuestionnaireCommentThread ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Questionnaire.QuestionnaireComment

entityName = "questionnaire_comment_thread"

findQuestionnaireCommentThreads :: String -> AppContextM [QuestionnaireCommentThread]
findQuestionnaireCommentThreads qtnUuid = do
  let sql =
        fromString
          "SELECT questionnaire_comment_thread.*, \
          \       (SELECT array_agg(concat(uuid, ':::::', text, ':::::', comment_thread_uuid, ':::::', created_by, ':::::', \
          \                                created_at, ':::::', updated_at)) AS comments \
          \        FROM questionnaire_comment \
          \        WHERE questionnaire_comment.comment_thread_uuid = questionnaire_comment_thread.uuid \
          \        GROUP BY comment_thread_uuid) AS comments \
          \FROM questionnaire_comment_thread \
          \WHERE questionnaire_uuid = ?"
  let params = [toField qtnUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findQuestionnaireCommentThreadById :: String -> AppContextM (Maybe QuestionnaireCommentThread)
findQuestionnaireCommentThreadById uuid = do
  let sql =
        fromString
          "SELECT questionnaire_comment_thread.*, \
          \       (SELECT array_agg(concat(uuid, ':::::', text, ':::::', comment_thread_uuid, ':::::', created_by, ':::::', \
          \                                created_at, ':::::', updated_at)) AS comments \
          \        FROM questionnaire_comment \
          \        WHERE questionnaire_comment.comment_thread_uuid = questionnaire_comment_thread.uuid \
          \        GROUP BY comment_thread_uuid) AS comments \
          \FROM questionnaire_comment_thread \
          \WHERE uuid = ?"
  let params = [toField uuid]
  logQuery sql params
  let action conn = query conn sql params
  entities <- runDB action
  case entities of
    [] -> return Nothing
    [entity] -> return . Just $ entity
    _ ->
      throwError $
      GeneralServerError
        (f'
           "createFindEntityByFn: find more entities found than one (entity: %s, param: %s)"
           [entityName, show [("uuid", uuid)]])

insertQuestionnaireCommentThread :: QuestionnaireCommentThread -> AppContextM Int64
insertQuestionnaireCommentThread = createInsertFn entityName

updateQuestionnaireCommentThreadById :: QuestionnaireCommentThread -> AppContextM QuestionnaireCommentThread
updateQuestionnaireCommentThreadById app = do
  now <- liftIO getCurrentTime
  let updatedApp = app & updatedAt .~ now
  let sql =
        fromString
          "UPDATE questionnaire_comment_thread SET uuid = ?, text = ?, questionnaire_uuid = ?, created_by = ?, created_at = ?, updated_at = ? WHERE uuid = ?"
  let params = toRow updatedApp ++ [toField $ updatedApp ^. uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  return updatedApp

updateQuestionnaireCommentThreadResolvedById :: U.UUID -> Bool -> AppContextM Int64
updateQuestionnaireCommentThreadResolvedById uuid resolved = do
  let sql = fromString "UPDATE questionnaire_comment_thread SET resolved = ?, updated_at = now() WHERE uuid = ?"
  let params = [toField resolved, toField uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteQuestionnaireCommentThreads :: AppContextM Int64
deleteQuestionnaireCommentThreads = createDeleteEntitiesFn entityName

deleteQuestionnaireCommentThreadById :: U.UUID -> AppContextM Int64
deleteQuestionnaireCommentThreadById uuid = createDeleteEntityByFn entityName [("uuid", U.toString uuid)]
