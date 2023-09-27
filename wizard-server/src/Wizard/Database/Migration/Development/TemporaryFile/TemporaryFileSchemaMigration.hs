module Wizard.Database.Migration.Development.TemporaryFile.TemporaryFileSchemaMigration where

import Database.PostgreSQL.Simple

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/TemporaryFile) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/TemporaryFile) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/TemporaryFile) drop tables"
  let sql = "drop table if exists temporary_file cascade;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/TemporaryFile) create table"
  let sql =
        "create table temporary_file \
        \     ( \
        \         uuid             uuid not null \
        \             constraint temporary_file_pk \
        \                 primary key, \
        \         file_name        varchar not null, \
        \         content_type     varchar not null, \
        \         expires_at       timestamp with time zone not null, \
        \         tenant_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \           constraint temporary_file_tenant_uuid_fk \
        \             references tenant, \
        \         created_by       uuid not null \
        \           constraint temporary_file_user_entity_uuid_fk \
        \             references user_entity on delete cascade, \
        \         created_at      timestamp with time zone not null \
        \     ); \
        \ create unique index temporary_file_uuid_uindex \
        \     on temporary_file (uuid);"
  let action conn = execute_ conn sql
  runDB action
