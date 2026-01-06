module Wizard.Database.DAO.Project.ProjectMigrationDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Project.Migration.ProjectMigration ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Project.Migration.ProjectMigration

entityName = "project_migration"

pageLabel = "migrations"

findProjectMigrations :: AppContextM [ProjectMigration]
findProjectMigrations = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findProjectMigrationsByOldProjectUuid :: U.UUID -> AppContextM [ProjectMigration]
findProjectMigrationsByOldProjectUuid oldProjectUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("old_project_uuid", U.toString oldProjectUuid)]

findProjectMigrationByNewProjectUuid :: U.UUID -> AppContextM ProjectMigration
findProjectMigrationByNewProjectUuid newProjectUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("new_project_uuid", U.toString newProjectUuid)]

findProjectMigrationByNewProjectUuid' :: U.UUID -> AppContextM (Maybe ProjectMigration)
findProjectMigrationByNewProjectUuid' newProjectUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("new_project_uuid", U.toString newProjectUuid)]

insertProjectMigration :: ProjectMigration -> AppContextM Int64
insertProjectMigration ms = do
  let sql =
        fromString
          "INSERT INTO project_migration VALUES (?, ?, ?::uuid[], ?)"
  let params = toRow ms
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

updateProjectMigrationByNewProjectUuid :: ProjectMigration -> AppContextM Int64
updateProjectMigrationByNewProjectUuid ms = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "UPDATE project_migration SET old_project_uuid = ?, new_project_uuid = ?, resolved_question_uuids = ?::uuid[], tenant_uuid = ? WHERE tenant_uuid = ? AND new_project_uuid = ?"
  let params = toRow ms ++ [toField tenantUuid, toField ms.newProjectUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteProjectMigrations :: AppContextM Int64
deleteProjectMigrations = createDeleteEntitiesFn entityName

deleteProjectMigrationByNewProjectUuid :: U.UUID -> AppContextM Int64
deleteProjectMigrationByNewProjectUuid newProjectUuid = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("new_project_uuid", U.toString newProjectUuid)]
