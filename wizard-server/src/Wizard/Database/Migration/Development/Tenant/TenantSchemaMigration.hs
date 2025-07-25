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
        "DROP TABLE IF EXISTS config_owl;\
        \DROP TABLE IF EXISTS config_mail;\
        \DROP TABLE IF EXISTS config_features;\
        \DROP TABLE IF EXISTS config_submission_service_supported_format;\
        \DROP TABLE IF EXISTS config_submission_service_request_header;\
        \DROP TABLE IF EXISTS config_submission_service;\
        \DROP TABLE IF EXISTS config_submission;\
        \DROP TABLE IF EXISTS config_questionnaire;\
        \DROP TABLE IF EXISTS config_knowledge_model_public_package_pattern;\
        \DROP TABLE IF EXISTS config_knowledge_model;\
        \DROP TABLE IF EXISTS config_registry;\
        \DROP TABLE IF EXISTS config_look_and_feel_custom_menu_link; \
        \DROP TABLE IF EXISTS config_look_and_feel; \
        \DROP TABLE IF EXISTS config_dashboard_and_login_screen_announcement; \
        \DROP TABLE IF EXISTS config_dashboard_and_login_screen; \
        \DROP TABLE IF EXISTS config_privacy_and_support; \
        \DROP TABLE IF EXISTS config_authentication_openid; \
        \DROP TABLE IF EXISTS config_authentication; \
        \DROP TABLE IF EXISTS config_organization; \
        \DROP TYPE IF EXISTS config_dashboard_and_login_screen_announcement_type;"
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
  createTcOrganizationTable
  createTcAuthenticationTable
  createTcInternalAuthenticationOpenIdTable
  createTcPrivacyAndSupportTable
  createTcDashboardAndLoginScreenTable
  createTcDashboardAndLoginScreenAnnouncementTable
  createTcLookAndFeelTable
  createTcLookAndFeelCustomMenuLinkTable
  createTcRegistryTable
  createTcKnowledgeModelTable
  createTcKnowledgeModelPublicPackagePatternTable
  createTcQuestionnaireTable
  createTcSubmissionTable
  createTcFeaturesTable
  createTcMailTable
  createTcOwlTable

createTcOrganizationTable = do
  logInfo _CMP_MIGRATION "(Table/ConfigOrganization) create tables"
  let sql =
        "CREATE TABLE config_organization \
        \( \
        \    tenant_uuid     uuid        NOT NULL, \
        \    name            varchar     NOT NULL, \
        \    description     varchar     NOT NULL, \
        \    organization_id varchar     NOT NULL, \
        \    affiliations    varchar[]   NOT NULL, \
        \    created_at      timestamptz NOT NULL, \
        \    updated_at      timestamptz NOT NULL, \
        \    CONSTRAINT config_organization_pk PRIMARY KEY (tenant_uuid), \
        \    CONSTRAINT config_organization_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTcAuthenticationTable = do
  logInfo _CMP_MIGRATION "(Table/ConfigAuthentication) create tables"
  let sql =
        "CREATE TABLE config_authentication \
        \( \
        \    tenant_uuid                              uuid        NOT NULL, \
        \    default_role                             varchar     NOT NULL, \
        \    internal_registration_enabled            bool        NOT NULL, \
        \    internal_two_factor_auth_enabled         bool        NOT NULL, \
        \    internal_two_factor_auth_code_length     int         NOT NULL, \
        \    internal_two_factor_auth_code_expiration int         NOT NULL, \
        \    created_at                               timestamptz NOT NULL, \
        \    updated_at                               timestamptz NOT NULL, \
        \    CONSTRAINT config_authentication_pk PRIMARY KEY (tenant_uuid), \
        \    CONSTRAINT config_authentication_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTcInternalAuthenticationOpenIdTable = do
  logInfo _CMP_MIGRATION "(Table/ConfigInternalAuthenticationOpenId) create tables"
  let sql =
        "CREATE TABLE config_authentication_openid \
        \( \
        \    id               varchar     NOT NULL, \
        \    name             varchar     NOT NULL, \
        \    url              varchar     NOT NULL, \
        \    client_id        varchar     NOT NULL, \
        \    client_secret    varchar     NOT NULL, \
        \    parameters       jsonb       NOT NULL, \
        \    style_icon       varchar, \
        \    style_background varchar, \
        \    style_color      varchar, \
        \    tenant_uuid      uuid        NOT NULL, \
        \    created_at       timestamptz NOT NULL, \
        \    updated_at       timestamptz NOT NULL, \
        \    CONSTRAINT config_authentication_openid_pk PRIMARY KEY (id, tenant_uuid), \
        \    CONSTRAINT config_authentication_openid_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTcPrivacyAndSupportTable = do
  logInfo _CMP_MIGRATION "(Table/ConfigPrivacyAndSupport) create tables"
  let sql =
        "CREATE TABLE config_privacy_and_support \
        \( \
        \    tenant_uuid          uuid        NOT NULL, \
        \    privacy_url          varchar, \
        \    terms_of_service_url varchar, \
        \    support_email        varchar, \
        \    support_site_name    varchar, \
        \    support_site_url     varchar, \
        \    support_site_icon    varchar, \
        \    created_at           timestamptz NOT NULL, \
        \    updated_at           timestamptz NOT NULL, \
        \    CONSTRAINT config_privacy_and_support_pk PRIMARY KEY (tenant_uuid), \
        \    CONSTRAINT config_privacy_and_support_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTcDashboardAndLoginScreenTable = do
  logInfo _CMP_MIGRATION "(Table/ConfigDashboardAndLoginScreen) create tables"
  let sql =
        "CREATE TABLE config_dashboard_and_login_screen \
        \( \
        \    tenant_uuid        uuid        NOT NULL, \
        \    dashboard_type     varchar     NOT NULL, \
        \    login_info         varchar, \
        \    login_info_sidebar varchar, \
        \    created_at         timestamptz NOT NULL, \
        \    updated_at         timestamptz NOT NULL, \
        \    CONSTRAINT config_dashboard_and_login_screen_pk PRIMARY KEY (tenant_uuid), \
        \    CONSTRAINT config_dashboard_and_login_screen_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTcDashboardAndLoginScreenAnnouncementTable = do
  logInfo _CMP_MIGRATION "(Table/ConfigDashboardAndLoginScreenAnnouncement) create tables"
  let sql =
        "CREATE TYPE config_dashboard_and_login_screen_announcement_type AS ENUM ('InfoAnnouncementLevelType', 'WarningAnnouncementLevelType', 'CriticalAnnouncementLevelType'); \
        \CREATE TABLE config_dashboard_and_login_screen_announcement \
        \( \
        \    tenant_uuid  uuid                                                NOT NULL, \
        \    position     int                                                 NOT NULL, \
        \    content      varchar                                             NOT NULL, \
        \    level        config_dashboard_and_login_screen_announcement_type NOT NULL, \
        \    login_screen bool                                                NOT NULL, \
        \    dashboard    bool                                                NOT NULL, \
        \    created_at   timestamptz                                         NOT NULL, \
        \    updated_at   timestamptz                                         NOT NULL, \
        \    CONSTRAINT config_dashboard_and_login_screen_announcement_pk PRIMARY KEY (tenant_uuid, position), \
        \    CONSTRAINT config_dashboard_and_login_screen_announcement_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTcLookAndFeelTable = do
  logInfo _CMP_MIGRATION "(Table/ConfigConfigLookAndFeel) create tables"
  let sql =
        "CREATE TABLE config_look_and_feel \
        \( \
        \    tenant_uuid        uuid        NOT NULL, \
        \    app_title          varchar, \
        \    app_title_short    varchar, \
        \    logo_url           varchar, \
        \    primary_color      varchar, \
        \    illustration_color varchar, \
        \    created_at         timestamptz NOT NULL, \
        \    updated_at         timestamptz NOT NULL, \
        \    CONSTRAINT config_look_and_feel_pk PRIMARY KEY (tenant_uuid), \
        \    CONSTRAINT config_look_and_feel_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTcLookAndFeelCustomMenuLinkTable = do
  logInfo _CMP_MIGRATION "(Table/ConfigLookAndFeelCustomMenuLink) create tables"
  let sql =
        "CREATE TABLE config_look_and_feel_custom_menu_link \
        \( \
        \    tenant_uuid uuid        NOT NULL, \
        \    position    int         NOT NULL, \
        \    icon        varchar     NOT NULL, \
        \    title       varchar     NOT NULL, \
        \    url         varchar     NOT NULL, \
        \    new_window  bool        NOT NULL, \
        \    created_at  timestamptz NOT NULL, \
        \    updated_at  timestamptz NOT NULL, \
        \    CONSTRAINT config_look_and_feel_custom_menu_link_pk PRIMARY KEY (tenant_uuid, position), \
        \    CONSTRAINT config_look_and_feel_custom_menu_link_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTcRegistryTable = do
  logInfo _CMP_MIGRATION "(Table/ConfigRegistry) create tables"
  let sql =
        "CREATE TABLE config_registry \
        \( \
        \    tenant_uuid  uuid        NOT NULL, \
        \    enabled      boolean     NOT NULL, \
        \    token        varchar     NOT NULL, \
        \    created_at   timestamptz NOT NULL, \
        \    updated_at   timestamptz NOT NULL, \
        \    CONSTRAINT config_registry_pk PRIMARY KEY (tenant_uuid), \
        \    CONSTRAINT config_registry_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTcKnowledgeModelTable = do
  logInfo _CMP_MIGRATION "(Table/ConfigKnowledgeModel) create tables"
  let sql =
        "CREATE TABLE config_knowledge_model \
        \( \
        \    tenant_uuid        uuid        NOT NULL, \
        \    public_enabled     bool        NOT NULL, \
        \    integration_config varchar     NOT NULL, \
        \    created_at         timestamptz NOT NULL, \
        \    updated_at         timestamptz NOT NULL, \
        \    CONSTRAINT config_knowledge_model_pk PRIMARY KEY (tenant_uuid), \
        \    CONSTRAINT config_knowledge_model_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTcKnowledgeModelPublicPackagePatternTable = do
  logInfo _CMP_MIGRATION "(Table/ConfigKnowledgeModelPublicPackagePattern) create tables"
  let sql =
        "CREATE TABLE config_knowledge_model_public_package_pattern \
        \( \
        \    tenant_uuid     uuid        NOT NULL, \
        \    position        int         NOT NULL, \
        \    organization_id varchar, \
        \    km_id           varchar, \
        \    min_version     varchar, \
        \    max_version     varchar, \
        \    created_at      timestamptz NOT NULL, \
        \    updated_at      timestamptz NOT NULL, \
        \    CONSTRAINT config_knowledge_model_public_package_pattern_pk PRIMARY KEY (tenant_uuid, position), \
        \    CONSTRAINT config_knowledge_model_public_package_pattern_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTcQuestionnaireTable = do
  logInfo _CMP_MIGRATION "(Table/ConfigQuestionnaire) create tables"
  let sql =
        "CREATE TABLE config_questionnaire\
        \( \
        \    tenant_uuid               uuid        NOT NULL, \
        \    visibility_enabled        boolean     NOT NULL, \
        \    visibility_default_value  varchar     NOT NULL, \
        \    sharing_enabled           boolean     NOT NULL, \
        \    sharing_default_value     varchar     NOT NULL, \
        \    sharing_anonymous_enabled BOOLEAN     NOT NULL, \
        \    creation                  varchar     NOT NULL, \
        \    project_tagging_enabled   boolean     NOT NULL, \
        \    project_tagging_tags      varchar[]   NOT NULL, \
        \    summary_report            boolean     NOT NULL, \
        \    feedback_enabled          boolean     NOT NULL, \
        \    feedback_token            TEXT        NOT NULL, \
        \    feedback_owner            TEXT        NOT NULL, \
        \    feedback_repo             TEXT        NOT NULL, \
        \    created_at                timestamptz NOT NULL, \
        \    updated_at                timestamptz NOT NULL, \
        \    CONSTRAINT config_questionnaire_pk PRIMARY KEY (tenant_uuid), \
        \    CONSTRAINT config_questionnaire_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
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

createTcOwlTable = do
  logInfo _CMP_MIGRATION "(Table/ConfigOwl) create tables"
  let sql =
        "CREATE TABLE config_owl \
        \( \
        \    tenant_uuid         uuid        NOT NULL, \
        \    enabled             boolean     NOT NULL, \
        \    name                varchar     NOT NULL, \
        \    organization_id     varchar     NOT NULL, \
        \    km_id               varchar     NOT NULL, \
        \    version             varchar     NOT NULL, \
        \    previous_package_id varchar, \
        \    root_element        varchar     NOT NULL, \
        \    created_at          timestamptz NOT NULL, \
        \    updated_at          timestamptz NOT NULL, \
        \    CONSTRAINT config_owl_pk PRIMARY KEY (tenant_uuid), \
        \    CONSTRAINT config_owl_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTcFeaturesTable = do
  logInfo _CMP_MIGRATION "(Table/ConfigFeatures) create tables"
  let sql =
        "CREATE TABLE config_features \
        \( \
        \    tenant_uuid          uuid        NOT NULL, \
        \    ai_assistant_enabled bool        NOT NULL, \
        \    tours_enabled        bool        NOT NULL, \
        \    created_at           timestamptz NOT NULL, \
        \    updated_at           timestamptz NOT NULL, \
        \    CONSTRAINT config_features_pk PRIMARY KEY (tenant_uuid), \
        \    CONSTRAINT config_features_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTcMailTable = do
  logInfo _CMP_MIGRATION "(Table/ConfigMail) create tables"
  let sql =
        "CREATE TABLE config_mail \
        \( \
        \    tenant_uuid uuid        NOT NULL, \
        \    config_uuid uuid, \
        \    created_at  timestamptz NOT NULL, \
        \    updated_at  timestamptz NOT NULL, \
        \    CONSTRAINT config_mail_pk PRIMARY KEY (tenant_uuid), \
        \    CONSTRAINT config_mail_config_uuid_fk FOREIGN KEY (config_uuid) REFERENCES instance_config_mail (uuid) ON DELETE SET DEFAULT, \
        \    CONSTRAINT config_mail_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
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
