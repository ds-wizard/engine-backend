module Registry.Database.Migration.Development.Locale.LocaleSchemaMigration where

import Database.PostgreSQL.Simple

import Registry.Database.DAO.Common
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.Common.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Locale) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Locale) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Locale) drop table"
  let sql = "drop table if exists locale cascade;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/Locale) create table"
  let sql =
        "create table locale \
        \ ( \
        \     id                        varchar not null \
        \         constraint locale_pk \
        \             primary key, \
        \     name                      varchar not null,\
        \     description               varchar not null,\
        \     code                      varchar not null,\
        \     organization_id           varchar not null,\
        \     locale_id                 varchar not null,\
        \     version                   varchar not null,\
        \     default_locale            bool not null,\
        \     license                   varchar not null,\
        \     readme                    varchar not null,\
        \     recommended_app_version   varchar not null,\
        \     enabled                   bool not null,\
        \     tenant_uuid uuid default '00000000-0000-0000-0000-000000000000' not null,\
        \     created_at timestamp with time zone not null,\
        \     updated_at timestamp with time zone not null\
        \ ); \
        \  \
        \ create unique index locale_uuid_uindex \
        \     on locale (id);"
  let action conn = execute_ conn sql
  runDB action
