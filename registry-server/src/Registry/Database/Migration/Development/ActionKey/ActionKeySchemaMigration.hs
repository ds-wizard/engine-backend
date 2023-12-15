module Registry.Database.Migration.Development.ActionKey.ActionKeySchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Registry.Database.DAO.Common
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.Common.Util.Logger

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/ActionKey) drop tables"
  let sql = "DROP TABLE IF EXISTS action_key CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/ActionKey) create table"
  let sql =
        "CREATE TABLE action_key \
        \( \
        \    uuid        uuid                     NOT NULL, \
        \    identity    varchar                  NOT NULL, \
        \    type        varchar                  NOT NULL, \
        \    hash        varchar                  NOT NULL, \
        \    created_at  timestamptz NOT NULL, \
        \    tenant_uuid uuid                     NOT NULL, \
        \    CONSTRAINT action_key_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT action_key_identity_fk FOREIGN KEY (identity) REFERENCES organization (organization_id) \
        \); \
        \ \
        \CREATE UNIQUE INDEX action_key_hash_uindex ON action_key (hash);"
  let action conn = execute_ conn sql
  runDB action
