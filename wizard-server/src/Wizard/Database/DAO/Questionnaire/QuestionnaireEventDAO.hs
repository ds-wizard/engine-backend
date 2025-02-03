module Wizard.Database.DAO.Questionnaire.QuestionnaireEventDAO where

import Control.Monad.Reader (asks)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Questionnaire.Questionnaire ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireDetail ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireDetailPreview ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireDetailQuestionnaire ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireDetailSettings ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireEvent ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireEventBundle ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireList ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireSimple ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireSimpleWithPerm ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireSquash ()
import Wizard.Database.Mapping.Questionnaire.QuestionnaireSuggestion ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Questionnaire.QuestionnaireEvent

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

updateQuestionnaireEventsByQuestionnaireUuid :: U.UUID -> Bool -> [QuestionnaireEvent] -> AppContextM Int64
updateQuestionnaireEventsByQuestionnaireUuid qtnUuid squashed events = do
  tenantUuid <- asks currentTenantUuid
  let insertSql =
        if null events
          then ""
          else f' "INSERT INTO questionnaire_event VALUES %s;" [generateQuestionMarksForEntities events]
  let sql =
        fromString $
          f'
            "BEGIN TRANSACTION; \
            \DELETE FROM questionnaire_event WHERE tenant_uuid = ? AND questionnaire_uuid = ?; \
            \%s \
            \UPDATE questionnaire SET squashed = ?, updated_at = now() WHERE uuid = ?; \
            \COMMIT;"
            [insertSql]
  let params = [toField tenantUuid, toField . U.toText $ qtnUuid] ++ concatMap toRow events ++ [toField squashed, toField qtnUuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action

updateQuestionnaireEventsByQuestionnaireUuid' :: U.UUID -> Bool -> [QuestionnaireEvent] -> AppContextM Int64
updateQuestionnaireEventsByQuestionnaireUuid' qtnUuid squashed events = do
  tenantUuid <- asks currentTenantUuid
  let insertSql =
        if null events
          then ""
          else f' "INSERT INTO questionnaire_event VALUES %s;" [generateQuestionMarksForEntities events]
  let sql =
        fromString $
          f'
            "BEGIN TRANSACTION; \
            \DELETE FROM questionnaire_event WHERE tenant_uuid = ? AND questionnaire_uuid = ?; \
            \%s \
            \UPDATE questionnaire SET squashed = ? WHERE uuid = ?; \
            \COMMIT;"
            [insertSql]
  let params = [toField tenantUuid, toField . U.toText $ qtnUuid] ++ concatMap toRow events ++ [toField squashed, toField qtnUuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action

updateQuestionnaireEventsByQuestionnaireUuid'' :: U.UUID -> [QuestionnaireEvent] -> AppContextM Int64
updateQuestionnaireEventsByQuestionnaireUuid'' qtnUuid events = do
  tenantUuid <- asks currentTenantUuid
  let insertSql =
        if null events
          then ""
          else f' "INSERT INTO questionnaire_event VALUES %s;" [generateQuestionMarksForEntities events]
  let sql =
        fromString $
          f'
            "BEGIN TRANSACTION; \
            \DELETE FROM questionnaire_event WHERE tenant_uuid = ? AND questionnaire_uuid = ?; \
            \%s \
            \COMMIT;"
            [insertSql]
  let params = [toField tenantUuid, toField . U.toText $ qtnUuid] ++ concatMap toRow events
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action

deleteQuestionnaireEvents :: AppContextM Int64
deleteQuestionnaireEvents = createDeleteEntitiesFn entityName

deleteQuestionnaireEventsByQuestionnaireUuid :: U.UUID -> AppContextM Int64
deleteQuestionnaireEventsByQuestionnaireUuid questionnaireUuid = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("questionnaire_uuid", U.toString questionnaireUuid)]
