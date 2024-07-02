module Wizard.Database.DAO.Questionnaire.QuestionnaireCommentThreadDAO where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.Map.Strict as M
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Model.Error.Error
import Shared.Common.Util.Map (doubleGroupBy)
import Shared.Common.Util.String
import Shared.Common.Util.Uuid
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Questionnaire.QuestionnaireCommentThread ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireCommentThreadList ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Questionnaire.QuestionnaireComment
import Wizard.Model.Questionnaire.QuestionnaireCommentList

entityName = "questionnaire_comment_thread"

findQuestionnaireCommentThreads :: U.UUID -> AppContextM [QuestionnaireCommentThread]
findQuestionnaireCommentThreads qtnUuid = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "SELECT questionnaire_comment_thread.*, \
          \       (SELECT array_agg(concat(uuid, ':::::', text, ':::::', comment_thread_uuid, ':::::', tenant_uuid, ':::::', created_by, ':::::', created_at, ':::::', updated_at)) AS comments \
          \        FROM questionnaire_comment \
          \        WHERE questionnaire_comment.comment_thread_uuid = questionnaire_comment_thread.uuid \
          \        GROUP BY comment_thread_uuid) AS comments \
          \FROM questionnaire_comment_thread \
          \WHERE questionnaire_uuid = ? AND tenant_uuid = ?"
  let params = [toField qtnUuid, toField tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findQuestionnaireCommentThreadsForQuestionnaire :: U.UUID -> Maybe String -> Maybe Bool -> Bool -> AppContextM [QuestionnaireCommentThreadList]
findQuestionnaireCommentThreadsForQuestionnaire qtnUuid mPath mResolved editor = do
  tenantUuid <- asks currentTenantUuid
  let (pathCondition, pathParam) =
        case mPath of
          Just path -> ("AND path = ?", [toField path])
          Nothing -> ("", [])
  let resolvedCondition =
        case mResolved of
          Just True -> "AND resolved = true"
          Just False -> "AND resolved = false"
          Nothing -> ""
  let privateCondition =
        if editor
          then ""
          else "AND private = false"
  let sql =
        fromString $
          f''
            "SELECT thread.uuid, \
            \       thread.path, \
            \       thread.resolved, \
            \       thread.private, \
            \       thread.created_at, \
            \       thread.updated_at, \
            \       user_entity.uuid, \
            \       user_entity.first_name, \
            \       user_entity.last_name, \
            \       user_entity.email, \
            \       user_entity.image_url, \
            \       (SELECT array_agg(concat(comment.uuid, '<:::::>', \
            \                                comment.text, '<:::::>', \
            \                                comment.created_at, '<:::::>', \
            \                                comment.updated_at, '<:::::>', \
            \                                user_entity.uuid, '<:::::>', \
            \                                user_entity.first_name, '<:::::>', \
            \                                user_entity.last_name, '<:::::>', \
            \                                user_entity.email, '<:::::>', \
            \                                user_entity.image_url \
            \                        )) AS comments \
            \         FROM questionnaire_comment comment \
            \         LEFT JOIN user_entity ON user_entity.uuid = comment.created_by \
            \         WHERE comment.comment_thread_uuid = thread.uuid \
            \         GROUP BY comment_thread_uuid) AS comments \
            \FROM questionnaire_comment_thread thread \
            \LEFT JOIN user_entity ON user_entity.uuid = thread.created_by \
            \WHERE thread.questionnaire_uuid = ? \
            \  AND thread.tenant_uuid = ? \
            \  ${pathCondition} \
            \  ${resolvedCondition} \
            \  ${privateCondition}"
            [ ("pathCondition", pathCondition)
            , ("resolvedCondition", resolvedCondition)
            , ("privateCondition", privateCondition)
            ]
  let params = toField qtnUuid : toField tenantUuid : pathParam
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findQuestionnaireCommentThreadsSimple :: U.UUID -> Bool -> Bool -> AppContextM (M.Map String (M.Map U.UUID Int))
findQuestionnaireCommentThreadsSimple qtnUuid resolved editor = do
  tenantUuid <- asks currentTenantUuid
  let privateCondition =
        if editor
          then ""
          else "AND qtn_cmnt_thr.private = false"
  let sql =
        fromString $
          f''
            "SELECT path, qtn_cmnt_thr.uuid::text, count(qtn_cmnt.uuid)::text \
            \FROM questionnaire_comment_thread qtn_cmnt_thr \
            \LEFT JOIN questionnaire_comment qtn_cmnt ON qtn_cmnt_thr.uuid = qtn_cmnt.comment_thread_uuid AND qtn_cmnt_thr.tenant_uuid = qtn_cmnt.tenant_uuid \
            \WHERE qtn_cmnt_thr.questionnaire_uuid = ? \
            \  AND qtn_cmnt_thr.tenant_uuid = ? \
            \  AND qtn_cmnt_thr.resolved = ? \
            \  ${privateCondition} \
            \GROUP BY path, qtn_cmnt_thr.uuid"
            [("privateCondition", privateCondition)]
  let params = [toField qtnUuid, toField tenantUuid, toField resolved]
  logQuery sql params
  let action conn = query conn sql params
  results <- runDB action
  return $ doubleGroupBy id u' read results

findQuestionnaireCommentThreadById :: U.UUID -> AppContextM (Maybe QuestionnaireCommentThread)
findQuestionnaireCommentThreadById uuid = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "SELECT questionnaire_comment_thread.*, \
          \       (SELECT array_agg(concat(uuid, ':::::', text, ':::::', comment_thread_uuid, ':::::', tenant_uuid, ':::::', created_by, ':::::', created_at, ':::::', updated_at)) AS comments \
          \        FROM questionnaire_comment \
          \        WHERE questionnaire_comment.comment_thread_uuid = questionnaire_comment_thread.uuid \
          \        GROUP BY comment_thread_uuid) AS comments \
          \FROM questionnaire_comment_thread \
          \WHERE uuid = ? AND tenant_uuid = ?"
  let params = [toField uuid, toField tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  entities <- runDB action
  case entities of
    [] -> return Nothing
    [entity] -> return . Just $ entity
    _ ->
      throwError $
        GeneralServerError
          ( f'
              "createFindEntityByFn: find more entities found than one (entity: %s, param: %s)"
              [entityName, show [("uuid", uuid)]]
          )

insertQuestionnaireCommentThread :: QuestionnaireCommentThread -> AppContextM Int64
insertQuestionnaireCommentThread = createInsertFn entityName

updateQuestionnaireCommentThreadById :: QuestionnaireCommentThread -> AppContextM QuestionnaireCommentThread
updateQuestionnaireCommentThreadById entity = do
  tenantUuid <- asks currentTenantUuid
  now <- liftIO getCurrentTime
  let updatedEntity = entity {updatedAt = now} :: QuestionnaireCommentThread
  let sql =
        fromString
          "UPDATE questionnaire_comment_thread SET uuid = ?, text = ?, questionnaire_uuid = ?, created_by = ?, created_at = ?, updated_at = ?, tenant_uuid = ? WHERE uuid = ? AND tenant_uuid = ?"
  let params = toRow updatedEntity ++ [toField updatedEntity.uuid, toField tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  return updatedEntity

updateQuestionnaireCommentThreadResolvedById :: U.UUID -> Bool -> AppContextM Int64
updateQuestionnaireCommentThreadResolvedById uuid resolved = do
  tenantUuid <- asks currentTenantUuid
  let sql = fromString "UPDATE questionnaire_comment_thread SET resolved = ?, updated_at = now() WHERE uuid = ? AND tenant_uuid = ?"
  let params = [toField resolved, toField uuid, toField tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteQuestionnaireCommentThreads :: AppContextM Int64
deleteQuestionnaireCommentThreads = createDeleteEntitiesFn entityName

deleteQuestionnaireCommentThreadById :: U.UUID -> AppContextM Int64
deleteQuestionnaireCommentThreadById uuid = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntityByFn entityName [("uuid", U.toString uuid), ("tenant_uuid", U.toString tenantUuid)]
