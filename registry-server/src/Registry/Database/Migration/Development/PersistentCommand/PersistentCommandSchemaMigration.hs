module Registry.Database.Migration.Development.PersistentCommand.PersistentCommandSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Registry.Database.DAO.Common
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.Common.Util.Logger

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/PersistentCommand) drop tables"
  let sql = "DROP TABLE IF EXISTS persistent_command CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/PersistentCommand) create table"
  let sql =
        "CREATE TABLE persistent_command \
        \( \
        \    uuid               uuid        NOT NULL, \
        \    state              varchar     NOT NULL, \
        \    component          varchar     NOT NULL, \
        \    function           varchar     NOT NULL, \
        \    body               varchar     NOT NULL, \
        \    last_error_message varchar, \
        \    attempts           int         NOT NULL, \
        \    max_attempts       int         NOT NULL, \
        \    tenant_uuid        uuid        NOT NULL, \
        \    created_by         varchar     NOT NULL, \
        \    created_at         timestamptz NOT NULL, \
        \    updated_at         timestamptz NOT NULL, \
        \    internal           bool        NOT NULL, \
        \    destination        varchar, \
        \    last_trace_uuid    uuid, \
        \    CONSTRAINT persistent_command_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT persistent_command_created_by_fk FOREIGN KEY (created_by) REFERENCES organization (organization_id) \
        \);"
  let action conn = execute_ conn sql
  runDB action
