module Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Migration.Questionnaire.MigratorState ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Migration.Questionnaire.MigratorState

entityName = "questionnaire_migration"

pageLabel = "migrations"

findMigratorStates :: AppContextM [MigratorState]
findMigratorStates = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findMigratorStatesByOldQuestionnaireUuid :: U.UUID -> AppContextM [MigratorState]
findMigratorStatesByOldQuestionnaireUuid oldQtnUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("old_questionnaire_uuid", U.toString oldQtnUuid)]

findMigratorStateByNewQuestionnaireUuid :: U.UUID -> AppContextM MigratorState
findMigratorStateByNewQuestionnaireUuid newQuestionnaireUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("new_questionnaire_uuid", U.toString newQuestionnaireUuid)]

findMigratorStateByNewQuestionnaireUuid' :: U.UUID -> AppContextM (Maybe MigratorState)
findMigratorStateByNewQuestionnaireUuid' newQuestionnaireUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("new_questionnaire_uuid", U.toString newQuestionnaireUuid)]

insertMigratorState :: MigratorState -> AppContextM Int64
insertMigratorState = createInsertFn entityName

updateMigratorStateByNewQuestionnaireUuid :: MigratorState -> AppContextM Int64
updateMigratorStateByNewQuestionnaireUuid ms = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "UPDATE questionnaire_migration SET old_questionnaire_uuid = ?, new_questionnaire_uuid = ?, resolved_question_uuids = ?, tenant_uuid = ? WHERE tenant_uuid = ? AND new_questionnaire_uuid = ?"
  let params = toRow ms ++ [toField tenantUuid, toField ms.newQuestionnaireUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteMigratorStates :: AppContextM Int64
deleteMigratorStates = createDeleteEntitiesFn entityName

deleteMigratorStateByNewQuestionnaireUuid :: U.UUID -> AppContextM Int64
deleteMigratorStateByNewQuestionnaireUuid newQuestionnaireUuid = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("new_questionnaire_uuid", U.toString newQuestionnaireUuid)]
