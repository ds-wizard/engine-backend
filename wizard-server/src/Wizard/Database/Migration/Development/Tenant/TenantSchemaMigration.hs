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
        \DROP TABLE IF EXISTS tenant;"
  let action conn = execute_ conn sql
  runDB action

dropConfigTables :: AppContextM Int64
dropConfigTables = do
  logInfo _CMP_MIGRATION "(Table/Config) drop table"
  let sql =
        "DROP TABLE IF EXISTS config_submission_service_supported_format;\
        \DROP TABLE IF EXISTS config_submission_service_request_header;\
        \DROP TABLE IF EXISTS config_submission_service;\
        \DROP TABLE IF EXISTS config_submission;\
        \DROP TABLE IF EXISTS tenant_config;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  createTenantTable
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

createConfigTables :: AppContextM Int64
createConfigTables = do
  createTenantConfigTable
  createTcSubmissionTable

createTenantConfigTable = do
  logInfo _CMP_MIGRATION "(Table/Config) create tables"
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

createTcSubmissionTable = do
  logInfo _CMP_MIGRATION "(Table/ConfigSubmission) create tables"
  let sql =
        "CREATE TABLE config_submission \
        \( \
        \    tenant_uuid uuid        NOT NULL, \
        \    enabled     boolean     NOT NULL, \
        \    created_at  timestamptz NOT NULL, \
        \    updated_at  timestamptz NOT NULL, \
        \    CONSTRAINT config_submission_pk PRIMARY KEY (tenant_uuid), \
        \    CONSTRAINT config_submission_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \CREATE TABLE config_submission_service \
        \( \
        \    tenant_uuid                 uuid        NOT NULL, \
        \    id                          varchar     NOT NULL, \
        \    name                        varchar     NOT NULL, \
        \    description                 varchar     NOT NULL, \
        \    props                       varchar[]   NOT NULL, \
        \    request_method              varchar     NOT NULL, \
        \    request_url                 varchar     NOT NULL, \
        \    request_multipart_enabled   boolean     NOT NULL, \
        \    request_multipart_file_name varchar     NOT NULL, \
        \    created_at                  timestamptz NOT NULL, \
        \    updated_at                  timestamptz NOT NULL, \
        \    CONSTRAINT config_submission_service_pk PRIMARY KEY (tenant_uuid, id), \
        \    CONSTRAINT config_submission_service_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \CREATE TABLE config_submission_service_request_header \
        \( \
        \    tenant_uuid uuid    NOT NULL, \
        \    service_id  varchar NOT NULL, \
        \    name        varchar NOT NULL, \
        \    value       varchar NOT NULL, \
        \    CONSTRAINT config_submission_service_request_header_pk PRIMARY KEY (tenant_uuid, service_id, name), \
        \    CONSTRAINT config_submission_service_request_header_service_id_fk FOREIGN KEY (service_id, tenant_uuid) REFERENCES config_submission_service (id, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT config_submission_service_request_header_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \CREATE TABLE config_submission_service_supported_format \
        \( \
        \    tenant_uuid uuid    NOT NULL, \
        \    service_id  varchar NOT NULL, \
        \    document_template_id varchar NOT NULL, \
        \    format_uuid uuid    NOT NULL, \
        \    CONSTRAINT config_submission_service_supported_format_pk PRIMARY KEY (tenant_uuid, service_id, document_template_id, format_uuid), \
        \    CONSTRAINT config_submission_service_supported_format_service_id_fk FOREIGN KEY (service_id, tenant_uuid) REFERENCES config_submission_service (id, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT config_submission_service_supported_format_document_template_id_fk FOREIGN KEY (document_template_id, tenant_uuid) REFERENCES document_template (id, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT config_submission_service_supported_format_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
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
