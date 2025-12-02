module Wizard.Database.DAO.KnowledgeModel.KnowledgeModelMigrationDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.KnowledgeModel.Migration.KnowledgeModelMigration ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.KnowledgeModel.Migration.KnowledgeModelMigration

entityName = "knowledge_model_migration"

findMigratorStates :: AppContextM [KnowledgeModelMigration]
findMigratorStates = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findMigratorStateByEditorUuid :: U.UUID -> AppContextM KnowledgeModelMigration
findMigratorStateByEditorUuid editorUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("editor_uuid", U.toString editorUuid)]

findMigratorStateByEditorUuid' :: U.UUID -> AppContextM (Maybe KnowledgeModelMigration)
findMigratorStateByEditorUuid' editorUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("editor_uuid", U.toString editorUuid)]

insertMigratorState :: KnowledgeModelMigration -> AppContextM Int64
insertMigratorState = createInsertFn entityName

updateMigratorState :: KnowledgeModelMigration -> AppContextM Int64
updateMigratorState migration = do
  let sql =
        fromString
          "UPDATE knowledge_model_migration SET editor_uuid = ?, metamodel_version = ?, state = ?, editor_previous_package_id = ?, target_package_id = ?, editor_previous_package_events = ?, target_package_events = ?, result_events = ?, current_knowledge_model = ?, tenant_uuid = ?, created_at = ? WHERE tenant_uuid = ? AND editor_uuid = ?"
  let params = toRow migration ++ [toField migration.tenantUuid, toField migration.editorUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteMigratorStates :: AppContextM Int64
deleteMigratorStates = createDeleteEntitiesFn entityName

deleteMigratorStateByEditorUuid :: U.UUID -> AppContextM Int64
deleteMigratorStateByEditorUuid editorUuid = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("editor_uuid", U.toString editorUuid)]
