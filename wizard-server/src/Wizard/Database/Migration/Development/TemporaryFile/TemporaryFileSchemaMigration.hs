module Wizard.Database.Migration.Development.TemporaryFile.TemporaryFileSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/TemporaryFile) drop tables"
  let sql = "DROP TABLE IF EXISTS temporary_file CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/TemporaryFile) create table"
  let sql =
        "CREATE TABLE temporary_file \
        \( \
        \    uuid         uuid        NOT NULL, \
        \    file_name    varchar     NOT NULL, \
        \    content_type varchar     NOT NULL, \
        \    expires_at   timestamptz NOT NULL, \
        \    tenant_uuid  uuid        NOT NULL, \
        \    created_by   uuid        NOT NULL, \
        \    created_at   timestamptz NOT NULL, \
        \    CONSTRAINT temporary_file_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT temporary_file_created_by_fk FOREIGN KEY (created_by, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid), \
        \    CONSTRAINT temporary_file_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action
