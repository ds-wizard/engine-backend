module Wizard.Database.Migration.Development.Locale.LocaleSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Locale) drop table"
  let sql =
        "DROP TRIGGER IF EXISTS trigger_on_after_locale_delete ON locale; \
        \DROP TABLE IF EXISTS locale CASCADE;"
  let action conn = execute_ conn sql
  runDB action

dropTriggers :: AppContextM Int64
dropTriggers = do
  logInfo _CMP_MIGRATION "(Trigger/Locale) drop tables"
  let sql = "DROP TRIGGER IF EXISTS trigger_on_after_locale_delete ON locale;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Locale) create table"
  let sql =
        "CREATE TABLE locale\
        \(\
        \    id                      varchar     NOT NULL,\
        \    name                    varchar     NOT NULL,\
        \    description             varchar     NOT NULL,\
        \    code                    varchar     NOT NULL,\
        \    organization_id         varchar     NOT NULL,\
        \    locale_id               varchar     NOT NULL,\
        \    version                 varchar     NOT NULL,\
        \    default_locale          bool        NOT NULL,\
        \    license                 varchar     NOT NULL,\
        \    readme                  varchar     NOT NULL,\
        \    recommended_app_version varchar     NOT NULL,\
        \    enabled                 bool        NOT NULL,\
        \    tenant_uuid             uuid        NOT NULL,\
        \    created_at              timestamptz NOT NULL,\
        \    updated_at              timestamptz NOT NULL,\
        \    CONSTRAINT locale_pk PRIMARY KEY (id, tenant_uuid),\
        \    CONSTRAINT locale_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid)\
        \);"
  let action conn = execute_ conn sql
  runDB action

createTriggers :: AppContextM Int64
createTriggers = do
  logInfo _CMP_MIGRATION "(Trigger/Locale) create triggers"
  let sql =
        "CREATE OR REPLACE TRIGGER trigger_on_after_locale_delete \
        \    AFTER DELETE \
        \    ON locale \
        \    FOR EACH ROW \
        \EXECUTE FUNCTION create_persistent_command_from_entity_id('locale', 'deleteFromS3');"
  let action conn = execute_ conn sql
  runDB action
