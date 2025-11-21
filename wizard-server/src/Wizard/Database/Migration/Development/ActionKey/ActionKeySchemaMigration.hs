module Wizard.Database.Migration.Development.ActionKey.ActionKeySchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

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
        \    uuid        uuid        NOT NULL, \
        \    identity    uuid        NOT NULL, \
        \    type        varchar     NOT NULL, \
        \    hash        varchar     NOT NULL, \
        \    created_at  timestamptz NOT NULL, \
        \    tenant_uuid uuid        NOT NULL, \
        \    CONSTRAINT action_key_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT action_key_identity_fk FOREIGN KEY (identity) REFERENCES user_entity (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT action_key_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \ \
        \CREATE UNIQUE INDEX action_key_hash_uindex ON action_key (hash);"
  let action conn = execute_ conn sql
  runDB action
