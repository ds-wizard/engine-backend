module WizardLib.Public.Database.Migration.Development.ExternalLink.ExternalLinkSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Logger

dropTables :: AppContextC s sc m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/ExternalLink) drop tables"
  let sql = "DROP TABLE IF EXISTS external_link_usage CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextC s sc m => m Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/ExternalLink) create table"
  let sql =
        "CREATE TABLE external_link_usage \
        \( \
        \    uuid        uuid        NOT NULL, \
        \    url         varchar     NOT NULL, \
        \    tenant_uuid uuid        NOT NULL, \
        \    created_at  timestamptz NOT NULL, \
        \    CONSTRAINT external_link_usage_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT external_link_usage_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action
