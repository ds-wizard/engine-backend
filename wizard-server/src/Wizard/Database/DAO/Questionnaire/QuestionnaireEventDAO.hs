module Wizard.Database.DAO.Questionnaire.QuestionnaireEventDAO where

import Control.Monad (unless, void)
import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Model.Common.Lens
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Util.Logger
import Shared.Common.Util.String
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Questionnaire.QuestionnaireEvent ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireEventList ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireEventLenses ()
import Wizard.Model.Questionnaire.QuestionnaireEventList

entityName = "questionnaire_event"

pageLabel = "questionnaireEvents"

findQuestionnaireEventsPage :: U.UUID -> Pageable -> [Sort] -> AppContextM (Page QuestionnaireEventList)
findQuestionnaireEventsPage questionnaireUuid pageable sort = do
  -- 1. Prepare variables
  do
    tenantUuid <- asks (.tenantUuid')
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    -- 2. Get total count
    let countSql =
          fromString
            "SELECT count(*) \
            \FROM questionnaire_event \
            \WHERE tenant_uuid = ? AND questionnaire_uuid = ?"
    let countParams = [U.toString tenantUuid, U.toString questionnaireUuid]
    logQuery countSql countParams
    let action conn = query conn countSql countParams
    result <- runDB action
    let count =
          case result of
            [count] -> fromOnly count
            _ -> 0
    -- 3. Get entities
    let sql =
          fromString $
            f''
              "SELECT questionnaire_event.uuid, \
              \       questionnaire_event.event_type, \
              \       questionnaire_event.path, \
              \       questionnaire_event.created_at, \
              \       questionnaire_event.value_type, \
              \       questionnaire_event.value, \
              \       questionnaire_event.value_id, \
              \       questionnaire_event.value_raw, \
              \       questionnaire_event.created_by   AS created_by_uuid, \
              \       user_entity.first_name           AS created_by_first_name, \
              \       user_entity.last_name            AS created_by_last_name, \
              \       gravatar_hash(user_entity.email) AS created_by_gravatar_hash, \
              \       user_entity.image_url            AS created_by_email \
              \FROM questionnaire_event \
              \     LEFT JOIN user_entity ON user_entity.uuid = questionnaire_event.created_by \
              \WHERE questionnaire_event.tenant_uuid = ? \
              \  AND questionnaire_event.questionnaire_uuid = ? \
              \${sort} \
              \OFFSET ${offset} \
              \LIMIT ${limit}"
              [ ("sort", mapSort sort)
              , ("offset", show skip)
              , ("limit", show sizeI)
              ]
    let params = [U.toString tenantUuid, U.toString questionnaireUuid]
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
    return $ Page pageLabel metadata entities

findQuestionnaireEventsByQuestionnaireUuid :: U.UUID -> AppContextM [QuestionnaireEvent]
findQuestionnaireEventsByQuestionnaireUuid questionnaireUuid = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        "SELECT * \
        \FROM questionnaire_event \
        \WHERE tenant_uuid = ? AND questionnaire_uuid = ? \
        \ORDER BY created_at"
  let params = [U.toString tenantUuid, U.toString questionnaireUuid]
  logInfoI _CMP_DATABASE sql
  let action conn = query conn (fromString sql) params
  runDB action

findQuestionnaireEventListsByQuestionnaireUuid :: U.UUID -> AppContextM [QuestionnaireEventList]
findQuestionnaireEventListsByQuestionnaireUuid questionnaireUuid = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "SELECT questionnaire_event.uuid, \
          \       questionnaire_event.event_type, \
          \       questionnaire_event.path, \
          \       questionnaire_event.created_at, \
          \       questionnaire_event.value_type, \
          \       questionnaire_event.value, \
          \       questionnaire_event.value_id, \
          \       questionnaire_event.value_raw, \
          \       questionnaire_event.created_by   AS created_by_uuid, \
          \       user_entity.first_name           AS created_by_first_name, \
          \       user_entity.last_name            AS created_by_last_name, \
          \       gravatar_hash(user_entity.email) AS created_by_gravatar_hash, \
          \       user_entity.image_url            AS created_by_email \
          \FROM questionnaire_event \
          \     LEFT JOIN user_entity ON user_entity.uuid = questionnaire_event.created_by \
          \WHERE questionnaire_event.tenant_uuid = ? \
          \  AND questionnaire_event.questionnaire_uuid = ? \
          \ORDER BY created_at"
  let params = [U.toString tenantUuid, U.toString questionnaireUuid]
  logInfoI _CMP_DATABASE sql
  let action conn = query conn (fromString sql) params
  runDB action

findQuestionnaireEventByUuid :: U.UUID -> AppContextM QuestionnaireEvent
findQuestionnaireEventByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityWithFieldsByFn "*" False entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findQuestionnaireEventByUuid' :: U.UUID -> AppContextM (Maybe QuestionnaireEvent)
findQuestionnaireEventByUuid' uuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

insertQuestionnaireEvent :: QuestionnaireEvent -> AppContextM Int64
insertQuestionnaireEvent = createInsertFn entityName

insertQuestionnaireEventWithTimestampUpdate :: U.UUID -> QuestionnaireEvent -> AppContextM ()
insertQuestionnaireEventWithTimestampUpdate qtnUuid event = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString $
          f'
            "UPDATE questionnaire SET squashed = false, updated_at = now() WHERE tenant_uuid = ? AND uuid = ?; \
            \INSERT INTO questionnaire_event VALUES (%s)"
            [generateQuestionMarks' event]
  let params =
        [ toField tenantUuid
        , toField qtnUuid
        ]
          ++ toRow event
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action
  return ()

insertQuestionnaireEvents :: [QuestionnaireEvent] -> AppContextM Int64
insertQuestionnaireEvents events = do
  if null events
    then return 0
    else do
      tenantUuid <- asks currentTenantUuid
      let sql =
            fromString $
              f'
                "INSERT INTO questionnaire_event VALUES %s"
                [generateQuestionMarksForEntities events]
      let params = concatMap toRow events
      logInsertAndUpdate sql params
      let action conn = execute conn sql params
      runDB action

updateQuestionnaireEventByUuid :: QuestionnaireEvent -> AppContextM Int64
updateQuestionnaireEventByUuid event = do
  tenantUuid <- asks currentTenantUuid
  let sql = fromString "UPDATE questionnaire_event SET uuid = ?, event_type = ?, path = ?, created_at = ?, created_by = ?, questionnaire_uuid = ?, tenant_uuid = ?, value_type = ?, value = ?, value_id = ?, value_raw = ? WHERE uuid = ? AND tenant_uuid = ?"
  let params = toRow event ++ [toField (getUuid event), toField tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

syncQuestionnaireEventsWithDb :: [QuestionnaireEvent] -> [QuestionnaireEvent] -> AppContextM ()
syncQuestionnaireEventsWithDb oldEvents newEvents = do
  let dbEventMap = map getUuid oldEvents
  let newEventMap = map getUuid newEvents
  let toDelete = dbEventMap L.\\ newEventMap
  let toInsert = filter (\e -> getUuid e `notElem` dbEventMap) newEvents
  let toUpdate = filter (\e -> e `notElem` oldEvents && getUuid e `elem` dbEventMap) newEvents
  unless (null toInsert) (void $ insertQuestionnaireEvents toInsert)
  traverse_ updateQuestionnaireEventByUuid toUpdate
  void $ deleteQuestionnaireEventsByUuids toDelete

deleteQuestionnaireEvents :: AppContextM Int64
deleteQuestionnaireEvents = createDeleteEntitiesFn entityName

deleteQuestionnaireEventsByUuids :: [U.UUID] -> AppContextM ()
deleteQuestionnaireEventsByUuids eventUuids =
  unless
    (null eventUuids)
    (void $ createDeleteEntityWhereInFn entityName "uuid" (fmap U.toString eventUuids))

deleteQuestionnaireEventsByQuestionnaireUuid :: U.UUID -> AppContextM Int64
deleteQuestionnaireEventsByQuestionnaireUuid questionnaireUuid = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("questionnaire_uuid", U.toString questionnaireUuid)]
