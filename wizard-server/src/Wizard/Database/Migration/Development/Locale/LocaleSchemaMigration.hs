module Wizard.Database.Migration.Development.Locale.LocaleSchemaMigration where

import Database.PostgreSQL.Simple

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Locale) started"
  dropFunctions
  dropTables
  createTables
  createFunctions
  logInfo _CMP_MIGRATION "(Table/Locale) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Locale) drop table"
  let sql = "drop table if exists locale cascade;"
  let action conn = execute_ conn sql
  runDB action

dropFunctions = do
  logInfo _CMP_MIGRATION "(Function/Locale) drop functions"
  let sql = "DROP FUNCTION IF EXISTS get_locale_state;"
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
        \     app_uuid                  uuid not null \
        \       constraint locale_app_uuid_fk \
        \         references app, \
        \     created_at timestamp with time zone not null,\
        \     updated_at timestamp with time zone not null\
        \ ); \
        \  \
        \ create unique index locale_uuid_uindex \
        \     on locale (id);"
  let action conn = execute_ conn sql
  runDB action

createFunctions = do
  logInfo _CMP_MIGRATION "(Function/Locale) create functions"
  createGetLocaleStateFn

createGetLocaleStateFn = do
  let sql =
        "CREATE or REPLACE FUNCTION get_locale_state(remote_version varchar, local_version varchar) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    state varchar; \
        \BEGIN \
        \    SELECT CASE \
        \               WHEN remote_version IS NULL THEN 'UnknownLocaleState' \
        \               WHEN compare_version(remote_version, local_version) = 'LT' THEN 'UnpublishedLocaleState' \
        \               WHEN compare_version(remote_version, local_version) = 'EQ' THEN 'UpToDateLocaleState' \
        \               WHEN compare_version(remote_version, local_version) = 'GT' THEN 'OutdatedLocaleState' \
        \               END \
        \    INTO state; \
        \    RETURN state; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  runDB action
