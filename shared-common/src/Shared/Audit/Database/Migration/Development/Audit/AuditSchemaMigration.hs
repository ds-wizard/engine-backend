module Shared.Audit.Database.Migration.Development.Audit.AuditSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Constant.Component
import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Logger

runMigration :: AppContextC s sc m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Audit) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Audit) ended"

dropTables :: AppContextC s sc m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Audit) drop tables"
  let sql = "DROP TABLE IF EXISTS audit CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextC s sc m => m Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Audit) create table"
  let sql =
        "CREATE TABLE audit \
        \( \
        \    uuid        uuid        NOT NULL, \
        \    component   varchar     NOT NULL, \
        \    action      varchar     NOT NULL, \
        \    entity      varchar     NOT NULL, \
        \    body        json        NOT NULL, \
        \    created_by  uuid, \
        \    tenant_uuid uuid        NOT NULL, \
        \    created_at  timestamptz NOT NULL, \
        \    CONSTRAINT audit_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT audit_created_by_fk FOREIGN KEY (created_by, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid), \
        \    CONSTRAINT audit_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action
