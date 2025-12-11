module Wizard.Database.DAO.Project.ProjectEventDAO where

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
import Wizard.Database.Mapping.Project.ProjectEvent ()
import Wizard.Database.Mapping.Project.ProjectEventList ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Project.Event.ProjectEvent
import Wizard.Model.Project.Event.ProjectEventLenses ()
import Wizard.Model.Project.Event.ProjectEventList

entityName = "project_event"

pageLabel = "projectEvents"

findProjectEventsPage :: U.UUID -> Pageable -> [Sort] -> AppContextM (Page ProjectEventList)
findProjectEventsPage projectUuid pageable sort = do
  -- 1. Prepare variables
  do
    tenantUuid <- asks (.tenantUuid')
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    -- 2. Get total count
    let countSql =
          fromString
            "SELECT count(*) \
            \FROM project_event \
            \WHERE tenant_uuid = ? AND project_uuid = ?"
    let countParams = [U.toString tenantUuid, U.toString projectUuid]
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
              "SELECT project_event.uuid, \
              \       project_event.event_type, \
              \       project_event.path, \
              \       project_event.created_at, \
              \       project_event.value_type, \
              \       project_event.value, \
              \       project_event.value_id, \
              \       project_event.value_raw, \
              \       project_event.created_by   AS created_by_uuid, \
              \       user_entity.first_name           AS created_by_first_name, \
              \       user_entity.last_name            AS created_by_last_name, \
              \       gravatar_hash(user_entity.email) AS created_by_gravatar_hash, \
              \       user_entity.image_url            AS created_by_email \
              \FROM project_event \
              \     LEFT JOIN user_entity ON user_entity.uuid = project_event.created_by \
              \WHERE project_event.tenant_uuid = ? \
              \  AND project_event.project_uuid = ? \
              \${sort} \
              \OFFSET ${offset} \
              \LIMIT ${limit}"
              [ ("sort", mapSort sort)
              , ("offset", show skip)
              , ("limit", show sizeI)
              ]
    let params = [U.toString tenantUuid, U.toString projectUuid]
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

findProjectEventsByProjectUuid :: U.UUID -> AppContextM [ProjectEvent]
findProjectEventsByProjectUuid projectUuid = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        "SELECT * \
        \FROM project_event \
        \WHERE tenant_uuid = ? AND project_uuid = ? \
        \ORDER BY created_at"
  let params = [U.toString tenantUuid, U.toString projectUuid]
  logInfoI _CMP_DATABASE sql
  let action conn = query conn (fromString sql) params
  runDB action

findProjectEventListsByProjectUuid :: U.UUID -> AppContextM [ProjectEventList]
findProjectEventListsByProjectUuid projectUuid = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "SELECT project_event.uuid, \
          \       project_event.event_type, \
          \       project_event.path, \
          \       project_event.created_at, \
          \       project_event.value_type, \
          \       project_event.value, \
          \       project_event.value_id, \
          \       project_event.value_raw, \
          \       project_event.created_by   AS created_by_uuid, \
          \       user_entity.first_name           AS created_by_first_name, \
          \       user_entity.last_name            AS created_by_last_name, \
          \       gravatar_hash(user_entity.email) AS created_by_gravatar_hash, \
          \       user_entity.image_url            AS created_by_email \
          \FROM project_event \
          \     LEFT JOIN user_entity ON user_entity.uuid = project_event.created_by \
          \WHERE project_event.tenant_uuid = ? \
          \  AND project_event.project_uuid = ? \
          \ORDER BY created_at"
  let params = [U.toString tenantUuid, U.toString projectUuid]
  logInfoI _CMP_DATABASE sql
  let action conn = query conn (fromString sql) params
  runDB action

findProjectEventByUuid :: U.UUID -> AppContextM ProjectEvent
findProjectEventByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityWithFieldsByFn "*" False entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findProjectEventByUuid' :: U.UUID -> AppContextM (Maybe ProjectEvent)
findProjectEventByUuid' uuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

insertProjectEvent :: ProjectEvent -> AppContextM Int64
insertProjectEvent = createInsertFn entityName

insertProjectEventWithTimestampUpdate :: U.UUID -> ProjectEvent -> AppContextM ()
insertProjectEventWithTimestampUpdate projectUuid event = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString $
          f'
            "UPDATE project SET squashed = false, updated_at = now() WHERE tenant_uuid = ? AND uuid = ?; \
            \INSERT INTO project_event VALUES (%s)"
            [generateQuestionMarks' event]
  let params =
        [ toField tenantUuid
        , toField projectUuid
        ]
          ++ toRow event
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action
  return ()

insertProjectEvents :: [ProjectEvent] -> AppContextM Int64
insertProjectEvents events = do
  if null events
    then return 0
    else do
      tenantUuid <- asks currentTenantUuid
      let sql =
            fromString $
              f'
                "INSERT INTO project_event VALUES %s"
                [generateQuestionMarksForEntities events]
      let params = concatMap toRow events
      logInsertAndUpdate sql params
      let action conn = execute conn sql params
      runDB action

updateProjectEventByUuid :: ProjectEvent -> AppContextM Int64
updateProjectEventByUuid event = do
  tenantUuid <- asks currentTenantUuid
  let sql = fromString "UPDATE project_event SET uuid = ?, event_type = ?, path = ?, created_at = ?, created_by = ?, project_uuid = ?, tenant_uuid = ?, value_type = ?, value = ?, value_id = ?, value_raw = ? WHERE uuid = ? AND tenant_uuid = ?"
  let params = toRow event ++ [toField (getUuid event), toField tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

syncProjectEventsWithDb :: [ProjectEvent] -> [ProjectEvent] -> AppContextM ()
syncProjectEventsWithDb oldEvents newEvents = do
  let dbEventMap = map getUuid oldEvents
  let newEventMap = map getUuid newEvents
  let toDelete = dbEventMap L.\\ newEventMap
  let toInsert = filter (\e -> getUuid e `notElem` dbEventMap) newEvents
  let toUpdate = filter (\e -> e `notElem` oldEvents && getUuid e `elem` dbEventMap) newEvents
  unless (null toInsert) (void $ insertProjectEvents toInsert)
  traverse_ updateProjectEventByUuid toUpdate
  void $ deleteProjectEventsByUuids toDelete

deleteProjectEvents :: AppContextM Int64
deleteProjectEvents = createDeleteEntitiesFn entityName

deleteProjectEventsByUuids :: [U.UUID] -> AppContextM ()
deleteProjectEventsByUuids eventUuids =
  unless
    (null eventUuids)
    (void $ createDeleteEntityWhereInFn entityName "uuid" (fmap U.toString eventUuids))

deleteProjectEventsByProjectUuid :: U.UUID -> AppContextM Int64
deleteProjectEventsByProjectUuid projectUuid = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("project_uuid", U.toString projectUuid)]
