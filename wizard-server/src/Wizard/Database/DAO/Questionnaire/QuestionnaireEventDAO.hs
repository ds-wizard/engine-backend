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
import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Questionnaire.QuestionnaireEvent ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireEventLenses ()

entityName = "questionnaire_event"

pageLabel = "questionnaireEvents"

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
  let sql = fromString "UPDATE questionnaire_event SET uuid = ?, event_type = ?, path = ?, created_at = ?, created_by = ?, questionnaire_uuid = ?, tenant_uuid = ?, value_type = ?, value = ?, value_id = ? WHERE uuid = ? AND tenant_uuid = ?"
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

clearQuestionnaireEventCreatedBy :: U.UUID -> AppContextM ()
clearQuestionnaireEventCreatedBy userUuid = do
  let sql = fromString "UPDATE questionnaire_event SET created_by = null WHERE created_by = ?"
  let params = [U.toString userUuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action
  return ()

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
