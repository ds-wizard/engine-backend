module Registry.Database.Migration.Development.PersistentCommand.PersistentCommandSchemaMigration where

import Database.PostgreSQL.Simple

import Registry.Database.DAO.Common
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/PersistentCommand) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/PersistentCommand) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/PersistentCommand) drop tables"
  let sql = "drop table if exists persistent_command cascade;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/PersistentCommand) create table"
  let sql =
        "create table persistent_command \
        \ ( \
        \     uuid uuid not null, \
        \     state varchar not null, \
        \     component varchar not null, \
        \     function varchar not null, \
        \     body varchar not null, \
        \     last_error_message varchar, \
        \     attempts int not null, \
        \     max_attempts int not null, \
        \     app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null, \
        \     created_by varchar not null \
        \       constraint persistent_command_created_by_fk \
        \         references organization, \
        \     created_at timestamptz not null, \
        \     updated_at timestamptz not null, \
        \     internal bool not null default true \
        \ ); \
        \  \
        \ create unique index persistent_command_uuid_uindex \
        \     on persistent_command (uuid); \
        \ alter table persistent_command \
        \     add constraint persistent_command_pk \
        \         primary key (uuid); "
  let action conn = execute_ conn sql
  runDB action