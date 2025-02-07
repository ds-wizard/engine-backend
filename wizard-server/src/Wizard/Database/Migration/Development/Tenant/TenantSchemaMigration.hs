module Wizard.Database.Migration.Development.Tenant.TenantSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Tenant) drop table"
  let sql =
        "DROP TABLE IF EXISTS tenant_limit_bundle; \
        \DROP TABLE IF EXISTS tenant_config;\
        \DROP TABLE IF EXISTS tenant;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  createTenantTable
  createTenantConfigTable
  createTenantLimitBundleTable

createTenantTable = do
  logInfo _CMP_MIGRATION "(Table/Tenant) create table"
  let sql =
        "CREATE TABLE tenant \
        \( \
        \    uuid             uuid        NOT NULL, \
        \    tenant_id        varchar     NOT NULL, \
        \    name             varchar     NOT NULL, \
        \    server_domain    varchar     NOT NULL, \
        \    client_url       varchar     NOT NULL, \
        \    enabled          bool        NOT NULL, \
        \    created_at       timestamptz NOT NULL, \
        \    updated_at       timestamptz NOT NULL, \
        \    server_url       varchar     NOT NULL, \
        \    admin_server_url varchar, \
        \    admin_client_url varchar, \
        \    integration_hub_server_url varchar, \
        \    integration_hub_client_url varchar, \
        \    analytics_server_url varchar, \
        \    analytics_client_url varchar, \
        \    signal_bridge_url varchar, \
        \    state varchar NOT NULL DEFAULT 'ReadyForUseTenantState', \
        \    CONSTRAINT tenant_pk PRIMARY KEY (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTenantConfigTable = do
  logInfo _CMP_MIGRATION "(Table/TenantConfig) create tables"
  let sql =
        "CREATE TABLE tenant_config \
        \( \
        \    uuid                       uuid        NOT NULL, \
        \    organization               jsonb       NOT NULL, \
        \    authentication             jsonb       NOT NULL, \
        \    privacy_and_support        jsonb       NOT NULL, \
        \    dashboard_and_login_screen jsonb       NOT NULL, \
        \    look_and_feel              jsonb       NOT NULL, \
        \    registry                   jsonb       NOT NULL, \
        \    knowledge_model            jsonb       NOT NULL, \
        \    questionnaire              jsonb       NOT NULL, \
        \    submission                 jsonb       NOT NULL, \
        \    created_at                 timestamptz NOT NULL, \
        \    updated_at                 timestamptz NOT NULL, \
        \    owl                        jsonb       NOT NULL, \
        \    mail_config_uuid           uuid, \
        \    ai_assistant               jsonb       NOT NULL, \
        \    CONSTRAINT tenant_config_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT tenant_config_mail_config_uuid_fk FOREIGN KEY (mail_config_uuid) REFERENCES instance_config_mail (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTenantLimitBundleTable = do
  logInfo _CMP_MIGRATION "(Table/TenantLimitBundle) create table"
  let sql =
        "CREATE TABLE tenant_limit_bundle \
        \( \
        \    uuid                     uuid        NOT NULL, \
        \    users                    integer     NOT NULL, \
        \    active_users             integer     NOT NULL, \
        \    knowledge_models         integer     NOT NULL, \
        \    branches                 integer     NOT NULL, \
        \    document_templates       integer     NOT NULL, \
        \    questionnaires           integer     NOT NULL, \
        \    documents                integer     NOT NULL, \
        \    storage                  bigint      NOT NULL, \
        \    created_at               timestamptz NOT NULL, \
        \    updated_at               timestamptz NOT NULL, \
        \    document_template_drafts integer     NOT NULL, \
        \    locales                  integer     NOT NULL, \
        \    CONSTRAINT tenant_limit_bundle_pk PRIMARY KEY (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action
