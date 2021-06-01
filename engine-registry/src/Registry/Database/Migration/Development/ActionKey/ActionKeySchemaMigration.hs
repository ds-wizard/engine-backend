module Registry.Database.Migration.Development.ActionKey.ActionKeySchemaMigration where

import Database.PostgreSQL.Simple

import Registry.Database.DAO.Common
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/ActionKey) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/ActionKey) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/ActionKey) drop tables"
  let sql = "drop table if exists action_key;"
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
         \         organization_id varchar, \
         \         type            varchar, \
         \         hash            varchar, \
         \         created_at      timestamp with time zone \
         \     ); \
         \     create unique index action_key_uuid_uindex \
         \         on action_key (uuid); \
         \     create unique index action_key_hash_uindex \
         \         on action_key (hash); "
  let action conn = execute_ conn sql
  runDB action
