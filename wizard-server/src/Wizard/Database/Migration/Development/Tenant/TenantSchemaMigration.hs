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
        \DROP TABLE IF EXISTS tenant_plan;\
        \DROP TABLE IF EXISTS tenant_config;\
        \DROP TABLE IF EXISTS tenant;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  createTenantTable
  createTenantConfigTable
  createTenantPlanTable
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
        \    reporting_server_url varchar, \
        \    reporting_client_url varchar, \
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
        \    organization               json        NOT NULL, \
        \    authentication             json        NOT NULL, \
        \    privacy_and_support        json        NOT NULL, \
        \    dashboard_and_login_screen json        NOT NULL, \
        \    look_and_feel              json        NOT NULL, \
        \    registry                   json        NOT NULL, \
        \    knowledge_model            json        NOT NULL, \
        \    questionnaire              json        NOT NULL, \
        \    submission                 json        NOT NULL, \
        \    created_at                 timestamptz NOT NULL, \
        \    updated_at                 timestamptz NOT NULL, \
        \    feature                    json        NOT NULL, \
        \    owl                        json        NOT NULL, \
        \    mail_config_uuid           uuid, \
        \    CONSTRAINT tenant_config_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT tenant_config_mail_config_uuid_fk FOREIGN KEY (mail_config_uuid) REFERENCES instance_config_mail (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTenantPlanTable = do
  logInfo _CMP_MIGRATION "(Table/TenantPlan) create table"
  let sql =
        "CREATE TABLE tenant_plan \
        \( \
        \    uuid        uuid        NOT NULL, \
        \    name        varchar     NOT NULL, \
        \    users       integer, \
        \    since       timestamptz, \
        \    until       timestamptz, \
        \    test        bool        NOT NULL, \
        \    tenant_uuid uuid        NOT NULL, \
        \    created_at  timestamptz NOT NULL, \
        \    updated_at  timestamptz NOT NULL, \
        \    CONSTRAINT tenant_plan_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT tenant_plan_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTenantLimitBundleTable = do
  logInfo _CMP_MIGRATION "(Table/TenantLimitBundle) create table"
  let sql =
        "CREATE TABLE tenant_limit_bundle \
        \( \
        \    uuid                     uuid        NOT NULL, \
        \    users                    integer, \
        \    active_users             integer, \
        \    knowledge_models         integer, \
        \    branches                 integer, \
        \    document_templates       integer, \
        \    questionnaires           integer, \
        \    documents                integer, \
        \    storage                  bigint, \
        \    created_at               timestamptz NOT NULL, \
        \    updated_at               timestamptz NOT NULL, \
        \    document_template_drafts integer, \
        \    locales                  integer, \
        \    CONSTRAINT tenant_limit_bundle_pk PRIMARY KEY (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action
