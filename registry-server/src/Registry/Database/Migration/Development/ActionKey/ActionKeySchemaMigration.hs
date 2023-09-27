module Registry.Database.Migration.Development.ActionKey.ActionKeySchemaMigration where

import Database.PostgreSQL.Simple

import Registry.Database.DAO.Common
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.Common.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/ActionKey) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/ActionKey) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/ActionKey) drop tables"
  let sql = "drop table if exists action_key cascade;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/ActionKey) create table"
  let sql =
        "create table action_key \
        \     ( \
        \         uuid            uuid not null \
        \             constraint action_key_pk \
        \                 primary key, \
        \         identity        varchar not null, \
        \         type            varchar not null, \
        \         hash            varchar not null, \
        \         created_at      timestamp with time zone not null, \
        \         tenant_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \     ); \
        \create unique index action_key_uuid_uindex \
        \     on action_key (uuid); \
        \create unique index action_key_hash_uindex \
        \     on action_key (hash); \
        \  \
        \alter table action_key \
        \    add constraint action_key_organization_id_fk \
        \       foreign key (identity) references organization (organization_id) on delete cascade;"
  let action conn = execute_ conn sql
  runDB action
