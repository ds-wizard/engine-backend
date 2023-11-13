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
  let sql = "DROP TABLE IF EXISTS locale CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/Locale) create table"
  let sql =
        "CREATE TABLE locale \
        \( \
        \    id                      varchar     NOT NULL, \
        \    name                    varchar     NOT NULL, \
        \    description             varchar     NOT NULL, \
        \    code                    varchar     NOT NULL, \
        \    organization_id         varchar     NOT NULL, \
        \    locale_id               varchar     NOT NULL, \
        \    version                 varchar     NOT NULL, \
        \    default_locale          bool        NOT NULL, \
        \    license                 varchar     NOT NULL, \
        \    readme                  varchar     NOT NULL, \
        \    recommended_app_version varchar     NOT NULL, \
        \    enabled                 bool        NOT NULL, \
        \    tenant_uuid             uuid        NOT NULL, \
        \    created_at              timestamptz NOT NULL, \
        \    updated_at              timestamptz NOT NULL, \
        \    CONSTRAINT locale_pk PRIMARY KEY (id) \
        \); \
        \ \
        \CREATE UNIQUE INDEX locale_uuid_uindex ON locale (id);"
  let action conn = execute_ conn sql
  runDB action
