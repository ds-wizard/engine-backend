module Registry.Database.Migration.Development.Locale.LocaleSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Registry.Database.DAO.Common
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.Common.Util.Logger

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Locale) drop table"
  let sql = "DROP TABLE IF EXISTS locale CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Locale) create table"
  let sql =
        "CREATE TABLE locale \
        \( \
        \    uuid                    uuid        NOT NULL, \
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
        \    CONSTRAINT locale_pk PRIMARY KEY (uuid) \
        \); \
        \ \
        \CREATE UNIQUE INDEX locale_organization_id_locale_id_version_uindex ON locale (organization_id, locale_id, version);"
  let action conn = execute_ conn sql
  runDB action
