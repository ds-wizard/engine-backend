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

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Logger
import Shared.Common.Util.Map (doubleGroupBy)
import Shared.Common.Util.String
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Questionnaire.QuestionnaireCommentThread ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireCommentThreadAssigned ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireCommentThreadList ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireCommentThreadNotification ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Questionnaire.QuestionnaireComment
import Wizard.Model.Questionnaire.QuestionnaireCommentList
import Wizard.Model.Questionnaire.QuestionnaireCommentThreadAssigned
import Wizard.Model.Questionnaire.QuestionnaireCommentThreadNotification

entityName = "questionnaire_comment_thread"

findAssignedQuestionnaireCommentThreadsPage :: Maybe String -> Maybe U.UUID -> Maybe Bool -> Pageable -> [Sort] -> AppContextM (Page QuestionnaireCommentThreadAssigned)
findAssignedQuestionnaireCommentThreadsPage mQuery mQuestionnaireUuid mResolved pageable sort = do
  -- 1. Prepare variables
  currentUser <- getCurrentUser
  tenantUuid <- asks currentTenantUuid
  let (qCondition, qRegex) =
        case mQuery of
          Just query -> (" AND comment.text ~* ?", [regex query])
          Nothing -> ("", [])
  let (questionnaireUuidCondition, questionnaireUuidParam) =
        case mQuestionnaireUuid of
          Just questionnaireUuid -> ("AND qtn.uuid = ?", [U.toString questionnaireUuid])
          Nothing -> ("", [])
  let resolvedCondition =
        case mResolved of
          Just True -> "AND thread.resolved = true"
          Just False -> "AND thread.resolved = false"
          Nothing -> ""
  let params =
        [U.toString tenantUuid, U.toString currentUser.uuid]
          ++ qRegex
          ++ questionnaireUuidParam
  let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
  -- 2. Get total count
  let countSql =
        fromString $
          f''
            "SELECT COUNT(DISTINCT thread.uuid) \
            \FROM questionnaire_comment_thread thread \
            \JOIN questionnaire qtn ON qtn.uuid = thread.questionnaire_uuid AND qtn.tenant_uuid = thread.tenant_uuid \
            \LEFT JOIN questionnaire_comment comment ON comment.comment_thread_uuid = thread.uuid AND comment.tenant_uuid = thread.tenant_uuid \
            \WHERE thread.tenant_uuid = ? \
            \  AND thread.assigned_to = ? \
            \  AND comment.uuid = (SELECT comment.uuid \
            \                      FROM questionnaire_comment comment \
            \                      WHERE comment.comment_thread_uuid = thread.uuid AND comment.tenant_uuid = thread.tenant_uuid \
            \                      ORDER BY comment.created_at \
            \                      LIMIT 1) \
            \  ${qCondition} \
            \  ${questionnaireUuidCondition} \
            \  ${resolvedCondition}"
            [ ("qCondition", qCondition)
            , ("questionnaireUuidCondition", questionnaireUuidCondition)
            , ("resolvedCondition", resolvedCondition)
            ]
  logQuery countSql params
  let action conn = query conn countSql params
  result <- runDB action
  let count =
        case result of
          [count] -> fromOnly count
          _ -> 0
  -- 3. Get entities
  let sql =
        fromString $
          f''
            "SELECT qtn.uuid, \
            \       qtn.name, \
            \       thread.uuid, \
            \       thread.path, \
            \       thread.resolved, \
            \       thread.private, \
            \       thread.updated_at, \
            \       comment.text AS comment_text, \
            \       u.uuid AS created_by_uuid, \
            \       u.first_name AS created_by_first_name, \
            \       u.last_name AS created_by_last_name, \
            \       u.email AS created_by_email, \
            \       u.image_url AS created_by_image_url \
            \FROM questionnaire_comment_thread thread \
            \JOIN questionnaire qtn ON qtn.uuid = thread.questionnaire_uuid AND qtn.tenant_uuid = thread.tenant_uuid \
            \LEFT JOIN user_entity u ON u.uuid = thread.created_by AND u.tenant_uuid = thread.tenant_uuid \
            \LEFT JOIN questionnaire_comment comment ON comment.comment_thread_uuid = thread.uuid AND comment.tenant_uuid = thread.tenant_uuid \
            \WHERE thread.tenant_uuid = ? \
            \  AND thread.assigned_to = ? \
            \  AND comment.uuid = (SELECT comment.uuid \
            \                      FROM questionnaire_comment comment \
            \                      WHERE comment.comment_thread_uuid = thread.uuid AND comment.tenant_uuid = thread.tenant_uuid \
            \                      ORDER BY comment.created_at \
            \                      LIMIT 1) \
            \  ${qCondition} \
            \  ${questionnaireUuidCondition} \
            \  ${resolvedCondition} \
            \${sort} \
            \OFFSET ${offset} \
            \LIMIT ${limit}"
            [ ("qCondition", qCondition)
            , ("questionnaireUuidCondition", questionnaireUuidCondition)
            , ("resolvedCondition", resolvedCondition)
            , ("sort", mapSort sort)
            , ("offset", show skip)
            , ("limit", show sizeI)
            ]
  logQuery sql params
  let action conn = query conn sql params
  entities <- runDB action
  -- 4. Constructor response
  let metadata =
        PageMetadata
          { size = sizeI
          , totalElements = count
          , totalPages = computeTotalPage count sizeI
          , number = pageI
          }
  return $ Page "commentThreads" metadata entities

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
            \       assigned_to_user.uuid, \
            \       assigned_to_user.first_name, \
            \       assigned_to_user.last_name, \
            \       assigned_to_user.email, \
            \       assigned_to_user.image_url, \
            \       created_by_user.uuid, \
            \       created_by_user.first_name, \
            \       created_by_user.last_name, \
            \       created_by_user.email, \
            \       created_by_user.image_url, \
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
            \LEFT JOIN user_entity assigned_to_user ON assigned_to_user.uuid = thread.assigned_to \
            \LEFT JOIN user_entity created_by_user ON created_by_user.uuid = thread.created_by \
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

findQuestionnaireCommentThreadsForNotifying :: AppContextM [QuestionnaireCommentThreadNotification]
findQuestionnaireCommentThreadsForNotifying = do
  let sql =
        "SELECT qtn.uuid, \
        \       qtn.name, \
        \       qtn.tenant_uuid, \
        \       thread.uuid, \
        \       thread.path, \
        \       thread.resolved, \
        \       thread.private, \
        \       assigned_to.uuid, \
        \       assigned_to.first_name, \
        \       assigned_to.last_name, \
        \       assigned_to.email, \
        \       assigned_by.uuid, \
        \       assigned_by.first_name, \
        \       assigned_by.last_name, \
        \       assigned_by.email, \
        \       (SELECT comment.text \
        \        FROM questionnaire_comment comment \
        \        WHERE comment.comment_thread_uuid = thread.uuid \
        \          AND comment.tenant_uuid = thread.tenant_uuid \
        \        ORDER BY comment.created_at \
        \        LIMIT 1) comment_text, \
        \        tenant.client_url, \
        \        tenant_config.look_and_feel ->> 'appTitle' AS app_title, \
        \        tenant_config.look_and_feel ->> 'logoUrl' AS logo_url, \
        \        tenant_config.look_and_feel ->> 'primaryColor' AS primary_color, \
        \        tenant_config.look_and_feel ->> 'illustrationColor' AS illustration_color, \
        \        tenant_config.privacy_and_support ->> 'supportEmail' as support_email, \
        \        tenant_config.mail_config_uuid as mail_config_uuid \
        \FROM questionnaire_comment_thread thread \
        \JOIN questionnaire qtn ON qtn.uuid = thread.questionnaire_uuid AND qtn.tenant_uuid = thread.tenant_uuid \
        \JOIN user_entity assigned_to ON assigned_to.uuid = thread.assigned_to AND assigned_to.tenant_uuid = thread.tenant_uuid \
        \LEFT JOIN user_entity assigned_by ON assigned_by.uuid = thread.assigned_by AND assigned_by.tenant_uuid = thread.tenant_uuid \
        \JOIN tenant ON tenant.uuid = thread.tenant_uuid \
        \JOIN tenant_config ON tenant_config.uuid = thread.tenant_uuid \
        \WHERE thread.notification_required = true"
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = query_ conn (fromString sql)
  runDB action

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

updateQuestionnaireCommentThreadAssignee :: U.UUID -> Maybe U.UUID -> Maybe U.UUID -> AppContextM Int64
updateQuestionnaireCommentThreadAssignee uuid assignedTo assignedBy = do
  tenantUuid <- asks currentTenantUuid
  let sql = fromString "UPDATE questionnaire_comment_thread SET assigned_to = ?, assigned_by = ?, notification_required = true, updated_at = now() WHERE uuid = ? AND tenant_uuid = ?"
  let params = [toField assignedTo, toField assignedBy, toField uuid, toField tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

clearQuestionnaireCommentThreadAssignedTo :: U.UUID -> AppContextM ()
clearQuestionnaireCommentThreadAssignedTo userUuid = do
  let sql = fromString "UPDATE questionnaire_comment_thread SET assigned_to = null, notification_required = false WHERE assigned_to = ?"
  let params = [toField userUuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action
  return ()

clearQuestionnaireCommentThreadAssignedBy :: U.UUID -> AppContextM ()
clearQuestionnaireCommentThreadAssignedBy userUuid = do
  let sql = fromString "UPDATE questionnaire_comment_thread SET assigned_by = null, notification_required = false WHERE assigned_by = ?"
  let params = [toField userUuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action
  return ()

unsetQuestionnaireCommentThreadNotificationRequired :: AppContextM ()
unsetQuestionnaireCommentThreadNotificationRequired = do
  let sql = "UPDATE questionnaire_comment_thread SET notification_required = false WHERE notification_required = true"
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = execute_ conn (fromString sql)
  runDB action
  return ()

deleteQuestionnaireCommentThreads :: AppContextM Int64
deleteQuestionnaireCommentThreads = createDeleteEntitiesFn entityName

deleteQuestionnaireCommentThreadById :: U.UUID -> AppContextM Int64
deleteQuestionnaireCommentThreadById uuid = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntityByFn entityName [("uuid", U.toString uuid), ("tenant_uuid", U.toString tenantUuid)]
