module Wizard.Database.Migration.Development.Questionnaire.MigratorSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Migration/Questionnaire) drop tables"
  let sql = "DROP TABLE IF EXISTS questionnaire_migration;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Migration/Questionnaire) create table"
  let sql =
        "CREATE TABLE questionnaire_migration \
        \( \
        \    old_questionnaire_uuid  uuid   NOT NULL, \
        \    new_questionnaire_uuid  uuid   NOT NULL, \
        \    resolved_question_uuids uuid[] NOT NULL, \
        \    tenant_uuid             uuid   NOT NULL, \
        \    CONSTRAINT questionnaire_migration_pk PRIMARY KEY (old_questionnaire_uuid, new_questionnaire_uuid), \
        \    CONSTRAINT questionnaire_migration_old_questionnaire_uuid_fk FOREIGN KEY (old_questionnaire_uuid, tenant_uuid) REFERENCES questionnaire (uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_migration_new_questionnaire_uuid_fk FOREIGN KEY (new_questionnaire_uuid, tenant_uuid) REFERENCES questionnaire (uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_migration_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action
