module Wizard.Database.Migration.Development.Project.ProjectMigrationSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Migration/Project) drop tables"
  let sql = "DROP TABLE IF EXISTS project_migration;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Migration/Project) create table"
  let sql =
        "CREATE TABLE project_migration \
        \( \
        \    old_project_uuid  uuid   NOT NULL, \
        \    new_project_uuid  uuid   NOT NULL, \
        \    resolved_question_uuids uuid[] NOT NULL, \
        \    tenant_uuid             uuid   NOT NULL, \
        \    CONSTRAINT project_migration_pk PRIMARY KEY (old_project_uuid, new_project_uuid), \
        \    CONSTRAINT project_migration_old_project_uuid_fk FOREIGN KEY (old_project_uuid) REFERENCES project (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT project_migration_new_project_uuid_fk FOREIGN KEY (new_project_uuid) REFERENCES project (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT project_migration_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action
