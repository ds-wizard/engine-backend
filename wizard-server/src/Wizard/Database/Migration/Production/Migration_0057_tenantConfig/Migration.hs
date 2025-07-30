module Wizard.Database.Migration.Production.Migration_0057_tenantConfig.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 57, mmName = "Refactor tenant_config", mmDescription = "Refactor tenant_config"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  createTcOrganizationTable dbPool
  createTcAuthenticationTable dbPool
  createTcAuthenticationOpenIdTable dbPool
  createTcPrivacyAndSupportTable dbPool
  createTcDashboardAndLoginScreenTable dbPool
  createTcDashboardAndLoginScreenTableAnnouncementTable dbPool
  createTcLookAndFeelTable dbPool
  createTcLookAndFeelCustomMenuLinkTable dbPool
  createTcRegistryTable dbPool
  createTcKnowledgeModelTable dbPool
  createTcKnowledgeModelPublicPackagePatternTable dbPool
  createTcQuestionnaireTable dbPool
  createTcSubmissionTable dbPool
  createTcSubmissionServiceTable dbPool
  createTcSubmissionServiceRequestHeaderTable dbPool
  createTcSubmissionServiceSupportedFormatTable dbPool
  createTcOwlTable dbPool
  createTcFeaturesTable dbPool
  createTcMailTable dbPool
  createGravatarFunction dbPool
  recreateGetBranchForkOfPackageIdFunction dbPool
  dropTenantConfigTable dbPool

createTcOrganizationTable dbPool = do
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
        \); \
        \INSERT INTO config_organization (tenant_uuid, name, description, organization_id, affiliations, created_at, updated_at) \
        \SELECT uuid, \
        \       (organization ->> 'name')::varchar, \
        \       (organization ->> 'description')::varchar, \
        \       (organization ->> 'organizationId')::varchar, \
        \       (SELECT coalesce(array_agg(value), ARRAY []::varchar[]) \
        \        FROM (SELECT jsonb_array_elements_text((outer_tenant_config.organization ->> 'affiliations')::jsonb) AS value \
        \              FROM tenant_config inner_tenant_config \
        \              WHERE inner_tenant_config.uuid = outer_tenant_config.uuid) sub), \
        \       created_at, \
        \       updated_at \
        \FROM tenant_config outer_tenant_config;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createTcAuthenticationTable dbPool = do
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
        \); \
        \INSERT INTO config_authentication (tenant_uuid, default_role, internal_registration_enabled, \
        \                                   internal_two_factor_auth_enabled, internal_two_factor_auth_code_length, \
        \                                   internal_two_factor_auth_code_expiration, created_at, updated_at) \
        \SELECT uuid, \
        \       (authentication ->> 'defaultRole')::varchar, \
        \       (((((authentication ->> 'internal')::jsonb) ->> 'registration')::jsonb) ->> 'enabled')::boolean, \
        \       (((((authentication ->> 'internal')::jsonb) ->> 'twoFactorAuth')::jsonb) ->> 'enabled')::boolean, \
        \       (((((authentication ->> 'internal')::jsonb) ->> 'twoFactorAuth')::jsonb) ->> 'codeLength')::int, \
        \       (((((authentication ->> 'internal')::jsonb) ->> 'twoFactorAuth')::jsonb) ->> 'expiration')::int, \
        \       created_at, \
        \       updated_at \
        \FROM tenant_config;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createTcAuthenticationOpenIdTable dbPool = do
  -- cspell: disable  (contains parameteres spelling error fix in migration)
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
        \); \
        \INSERT INTO config_authentication_openid (id, name, url, client_id, client_secret, parameters, style_icon, \
        \                                          style_background, style_color, tenant_uuid, created_at, updated_at) \
        \SELECT nested.json_content ->> 'id', \
        \       nested.json_content ->> 'name', \
        \       nested.json_content ->> 'url', \
        \       nested.json_content ->> 'clientId', \
        \       nested.json_content ->> 'clientSecret', \
        \       (nested.json_content ->> 'parameteres')::jsonb, \
        \       ((nested.json_content ->> 'style')::jsonb) ->> 'icon', \
        \       ((nested.json_content ->> 'style')::jsonb) ->> 'background', \
        \       ((nested.json_content ->> 'style')::jsonb) ->> 'color', \
        \       nested.tenant_uuid, \
        \       nested.created_at, \
        \       nested.updated_at \
        \FROM (SELECT tenant_config.uuid                                                                           as tenant_uuid, \
        \             (jsonb_array_elements(((tenant_config.authentication ->> 'external')::jsonb) -> 'services')) as json_content, \
        \             tenant_config.created_at, \
        \             tenant_config.updated_at \
        \      FROM tenant_config) nested;"
  -- cspell: enable
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createTcPrivacyAndSupportTable dbPool = do
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
        \); \
        \INSERT INTO config_privacy_and_support (tenant_uuid, privacy_url, terms_of_service_url, support_email, \
        \                                        support_site_name, support_site_url, support_site_icon, created_at, updated_at) \
        \SELECT uuid, \
        \       (privacy_and_support ->> 'privacyUrl')::varchar, \
        \       (privacy_and_support ->> 'termsOfServiceUrl')::varchar, \
        \       (privacy_and_support ->> 'supportEmail')::varchar, \
        \       (privacy_and_support ->> 'supportSiteUrl')::varchar, \
        \       (privacy_and_support ->> 'supportSiteIcon')::varchar, \
        \       (privacy_and_support ->> 'supportSiteName')::varchar, \
        \       created_at, \
        \       updated_at \
        \FROM tenant_config;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createTcDashboardAndLoginScreenTable dbPool = do
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
        \); \
        \INSERT INTO config_dashboard_and_login_screen (tenant_uuid, dashboard_type, login_info, login_info_sidebar, created_at, \
        \                                               updated_at) \
        \SELECT uuid, \
        \       (dashboard_and_login_screen ->> 'dashboardType')::varchar, \
        \       (dashboard_and_login_screen ->> 'loginInfo')::varchar, \
        \       (dashboard_and_login_screen ->> 'loginInfoSidebar')::varchar, \
        \       created_at, \
        \       updated_at \
        \FROM tenant_config;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createTcDashboardAndLoginScreenTableAnnouncementTable dbPool = do
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
        \); \
        \INSERT INTO config_dashboard_and_login_screen_announcement (tenant_uuid, position, content, level, login_screen, \
        \                                                            dashboard, created_at, updated_at) \
        \SELECT announcement.tenant_uuid, \
        \       announcement.idx, \
        \       (announcement.json_content ->> 'content')::varchar, \
        \       (announcement.json_content ->> 'level')::config_dashboard_and_login_screen_announcement_type, \
        \       (announcement.json_content ->> 'loginScreen')::bool, \
        \       (announcement.json_content ->> 'dashboard')::bool, \
        \       announcement.created_at, \
        \       announcement.updated_at \
        \FROM (SELECT uuid as tenant_uuid, \
        \             json_content, \
        \             idx, \
        \             created_at, \
        \             updated_at \
        \      FROM tenant_config, \
        \           jsonb_array_elements((dashboard_and_login_screen ->> 'announcements')::jsonb) with ordinality as t(json_content, idx)) announcement;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createTcLookAndFeelTable dbPool = do
  let sql =
        "CREATE TABLE config_look_and_feel \
        \( \
        \    tenant_uuid         uuid        NOT NULL, \
        \    app_title           varchar, \
        \    app_title_short     varchar, \
        \    logo_url            varchar, \
        \    primary_color       varchar, \
        \    illustrations_color varchar, \
        \    created_at          timestamptz NOT NULL, \
        \    updated_at          timestamptz NOT NULL, \
        \    CONSTRAINT config_look_and_feel_pk PRIMARY KEY (tenant_uuid), \
        \    CONSTRAINT config_look_and_feel_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \INSERT INTO config_look_and_feel (tenant_uuid, app_title, app_title_short, logo_url, primary_color, illustrations_color, \
        \                                  created_at, updated_at) \
        \SELECT uuid, \
        \       (look_and_feel ->> 'appTitle')::varchar, \
        \       (look_and_feel ->> 'appTitleShort')::varchar, \
        \       (look_and_feel ->> 'logoUrl')::varchar, \
        \       (look_and_feel ->> 'primaryColor')::varchar, \
        \       (look_and_feel ->> 'illustrationsColor')::varchar, \
        \       created_at, \
        \       updated_at \
        \FROM tenant_config;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createTcLookAndFeelCustomMenuLinkTable dbPool = do
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
        \); \
        \INSERT INTO config_look_and_feel_custom_menu_link (tenant_uuid, position, icon, title, url, new_window, created_at, \
        \                                                   updated_at) \
        \SELECT custom_menu_link.tenant_uuid, \
        \       custom_menu_link.idx, \
        \       (custom_menu_link.json_content ->> 'icon')::varchar, \
        \       (custom_menu_link.json_content ->> 'title')::varchar, \
        \       (custom_menu_link.json_content ->> 'url')::varchar, \
        \       (custom_menu_link.json_content ->> 'newWindow')::bool, \
        \       custom_menu_link.created_at, \
        \       custom_menu_link.updated_at \
        \FROM (SELECT uuid as tenant_uuid, \
        \             json_content, \
        \             idx, \
        \             created_at, \
        \             updated_at \
        \      FROM tenant_config, \
        \           jsonb_array_elements((look_and_feel ->> 'customMenuLinks')::jsonb) with ordinality as t(json_content, idx)) custom_menu_link;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createTcRegistryTable dbPool = do
  let sql =
        "CREATE TABLE config_registry \
        \( \
        \    tenant_uuid uuid        NOT NULL, \
        \    enabled     boolean     NOT NULL, \
        \    token       varchar     NOT NULL, \
        \    created_at  timestamptz NOT NULL, \
        \    updated_at  timestamptz NOT NULL, \
        \    CONSTRAINT config_registry_pk PRIMARY KEY (tenant_uuid), \
        \    CONSTRAINT config_registry_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \INSERT INTO config_registry (tenant_uuid, enabled, token, created_at, updated_at) \
        \SELECT uuid, \
        \       (registry ->> 'enabled')::boolean, \
        \       (registry ->> 'token')::varchar, \
        \       created_at, \
        \       updated_at \
        \FROM tenant_config;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createTcKnowledgeModelTable dbPool = do
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
        \); \
        \INSERT INTO config_knowledge_model (tenant_uuid, public_enabled, integration_config, created_at, updated_at) \
        \SELECT uuid, \
        \       (((knowledge_model ->> 'public')::jsonb) ->> 'enabled')::boolean, \
        \       (knowledge_model ->> 'integrationConfig')::varchar, \
        \       created_at, \
        \       updated_at \
        \FROM tenant_config;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createTcKnowledgeModelPublicPackagePatternTable dbPool = do
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
        \); \
        \INSERT INTO config_knowledge_model_public_package_pattern (tenant_uuid, position, organization_id, km_id, min_version, \
        \                                                           max_version, created_at, updated_at) \
        \SELECT nested.tenant_uuid, \
        \       nested.idx, \
        \       nested.json_content ->> 'orgId', \
        \       nested.json_content ->> 'kmId', \
        \       nested.json_content ->> 'minVersion', \
        \       nested.json_content ->> 'maxVersion', \
        \       nested.created_at, \
        \       nested.updated_at \
        \FROM (SELECT tenant_config.uuid                        as tenant_uuid, \
        \             json_content, \
        \             idx, \
        \             tenant_config.created_at, \
        \             tenant_config.updated_at \
        \      FROM tenant_config, \
        \           jsonb_array_elements(((knowledge_model ->> 'public')::jsonb) -> 'packages') with ordinality as t(json_content, idx)) nested;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createTcQuestionnaireTable dbPool = do
  let sql =
        "CREATE TABLE config_questionnaire \
        \( \
        \    tenant_uuid               uuid        NOT NULL, \
        \    visibility_enabled        boolean     NOT NULL, \
        \    visibility_default_value  varchar     NOT NULL, \
        \    sharing_enabled           boolean     NOT NULL, \
        \    sharing_default_value     varchar     NOT NULL, \
        \    sharing_anonymous_enabled boolean     NOT NULL, \
        \    creation                  varchar     NOT NULL, \
        \    project_tagging_enabled   boolean     NOT NULL, \
        \    project_tagging_tags      varchar[]   NOT NULL, \
        \    summary_report            boolean     NOT NULL, \
        \    feedback_enabled          boolean     NOT NULL, \
        \    feedback_token            varchar     NOT NULL, \
        \    feedback_owner            varchar     NOT NULL, \
        \    feedback_repo             varchar     NOT NULL, \
        \    created_at                timestamptz NOT NULL, \
        \    updated_at                timestamptz NOT NULL, \
        \    CONSTRAINT config_questionnaire_pk PRIMARY KEY (tenant_uuid), \
        \    CONSTRAINT config_questionnaire_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \INSERT INTO config_questionnaire (tenant_uuid, visibility_enabled, visibility_default_value, sharing_enabled, \
        \                                  sharing_default_value, sharing_anonymous_enabled, creation, project_tagging_enabled, \
        \                                  project_tagging_tags, summary_report, feedback_enabled, feedback_token, \
        \                                  feedback_owner, feedback_repo, created_at, updated_at) \
        \SELECT uuid, \
        \       (((questionnaire ->> 'questionnaireVisibility')::jsonb) ->> 'enabled')::boolean, \
        \       (((questionnaire ->> 'questionnaireVisibility')::jsonb) ->> 'defaultValue')::varchar, \
        \       (((questionnaire ->> 'questionnaireSharing')::jsonb) ->> 'enabled')::boolean, \
        \       (((questionnaire ->> 'questionnaireSharing')::jsonb) ->> 'defaultValue')::varchar, \
        \       (((questionnaire ->> 'questionnaireSharing')::jsonb) ->> 'anonymousEnabled')::boolean, \
        \       questionnaire ->> 'questionnaireCreation', \
        \       (((questionnaire ->> 'projectTagging')::jsonb) ->> 'enabled')::boolean, \
        \       (SELECT coalesce(array_agg(value), ARRAY []::varchar[]) \
        \        FROM (SELECT jsonb_array_elements_text((((outer_tenant_config.questionnaire ->> 'projectTagging')::jsonb) ->> \
        \                                                'tags')::jsonb) AS value \
        \              FROM tenant_config inner_tenant_config \
        \              WHERE inner_tenant_config.uuid = outer_tenant_config.uuid) sub), \
        \       (((questionnaire ->> 'summaryReport')::jsonb) ->> 'enabled')::boolean, \
        \       (((questionnaire ->> 'feedback')::jsonb) ->> 'enabled')::boolean, \
        \       ((questionnaire ->> 'feedback')::jsonb) ->> 'token', \
        \       ((questionnaire ->> 'feedback')::jsonb) ->> 'owner', \
        \       ((questionnaire ->> 'feedback')::jsonb) ->> 'repo', \
        \       created_at, \
        \       updated_at \
        \FROM tenant_config outer_tenant_config;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createTcSubmissionTable dbPool = do
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
        \INSERT INTO config_submission (tenant_uuid, enabled, created_at, updated_at) \
        \SELECT uuid, \
        \       (submission ->> 'enabled')::bool, \
        \       created_at, \
        \       updated_at \
        \FROM tenant_config;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createTcSubmissionServiceTable dbPool = do
  let sql =
        "CREATE TABLE config_submission_service \
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
        \INSERT INTO config_submission_service (tenant_uuid, id, name, description, props, request_method, request_url, \
        \                                       request_multipart_enabled, request_multipart_file_name, created_at, updated_at) \
        \SELECT service.tenant_uuid, \
        \       service.json_content ->> 'id', \
        \       service.json_content ->> 'name', \
        \       service.json_content ->> 'description', \
        \       ARRAY(SELECT jsonb_array_elements_text((service.json_content ->> 'props')::jsonb)), \
        \       ((service.json_content ->> 'request')::jsonb) ->> 'method', \
        \       ((service.json_content ->> 'request')::jsonb) ->> 'url', \
        \       (((((service.json_content ->> 'request')::jsonb) ->> 'multipart')::jsonb) ->> 'enabled')::boolean, \
        \       ((((service.json_content ->> 'request')::jsonb) ->> 'multipart')::jsonb) ->> 'fileName', \
        \       service.created_at, \
        \       service.updated_at \
        \FROM (SELECT uuid as tenant_uuid, \
        \             json_content, \
        \             idx, \
        \             created_at, \
        \             updated_at \
        \      FROM tenant_config, \
        \           jsonb_array_elements((submission ->> 'services')::jsonb) with ordinality as t(json_content, idx)) service;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createTcSubmissionServiceRequestHeaderTable dbPool = do
  let sql =
        "CREATE TABLE config_submission_service_request_header \
        \( \
        \    tenant_uuid uuid    NOT NULL, \
        \    service_id  varchar NOT NULL, \
        \    name        varchar NOT NULL, \
        \    value       varchar NOT NULL, \
        \    CONSTRAINT config_submission_service_request_header_pk PRIMARY KEY (tenant_uuid, service_id, name), \
        \    CONSTRAINT config_submission_service_request_header_service_id_fk FOREIGN KEY (service_id, tenant_uuid) REFERENCES config_submission_service (id, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT config_submission_service_request_header_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \INSERT INTO config_submission_service_request_header (tenant_uuid, service_id, name, value) \
        \SELECT service.tenant_uuid           AS tenant_uuid, \
        \       service.json_content ->> 'id' AS service_id, \
        \       key                           AS name, \
        \       value                         AS value \
        \FROM (SELECT uuid as tenant_uuid, \
        \             json_content \
        \      FROM tenant_config, \
        \           jsonb_array_elements((submission ->> 'services')::jsonb) json_content) service, jsonb_each_text((((service.json_content ->> 'request')::jsonb) ->> 'headers')::jsonb);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createTcSubmissionServiceSupportedFormatTable dbPool = do
  let sql =
        "CREATE TABLE config_submission_service_supported_format \
        \( \
        \    tenant_uuid          uuid    NOT NULL, \
        \    service_id           varchar NOT NULL, \
        \    document_template_id varchar NOT NULL, \
        \    format_uuid          uuid    NOT NULL, \
        \    CONSTRAINT config_submission_service_supported_format_pk PRIMARY KEY (tenant_uuid, service_id, document_template_id, format_uuid), \
        \    CONSTRAINT config_submission_service_supported_format_service_id_fk FOREIGN KEY (service_id, tenant_uuid) REFERENCES config_submission_service (id, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT config_submission_service_supported_format_dt_id_fk FOREIGN KEY (document_template_id, tenant_uuid) REFERENCES document_template (id, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT config_submission_service_supported_format_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \INSERT INTO config_submission_service_supported_format (tenant_uuid, service_id, document_template_id, format_uuid) \
        \SELECT service.tenant_uuid             AS tenant_uuid, \
        \       service.json_content ->> 'id'   AS service_id, \
        \       value ->> 'templateId'          AS document_template_id, \
        \       (value ->> 'formatUuid') ::uuid AS format_uuid \
        \FROM (SELECT uuid as tenant_uuid, \
        \             json_content \
        \      FROM tenant_config, \
        \           jsonb_array_elements((submission ->> 'services')::jsonb) json_content) service, jsonb_array_elements(((service.json_content ->> 'supportedFormats')::jsonb));"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createTcOwlTable dbPool = do
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
        \); \
        \INSERT INTO config_owl (tenant_uuid, enabled, name, organization_id, km_id, version, previous_package_id, root_element, \
        \                        created_at, updated_at) \
        \SELECT uuid, \
        \       (owl ->> 'enabled')::boolean, \
        \       coalesce(owl ->> 'name', ''), \
        \       coalesce(owl ->> 'organizationId', ''), \
        \       coalesce(owl ->> 'kmId', ''), \
        \       coalesce(owl ->> 'version', ''), \
        \       owl ->> 'previousPackageId', \
        \       coalesce(owl ->> 'rootElement', ''), \
        \       created_at, \
        \       updated_at \
        \FROM tenant_config;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createTcFeaturesTable dbPool = do
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
        \); \
        \INSERT INTO config_features (tenant_uuid, ai_assistant_enabled, tours_enabled, created_at, updated_at) \
        \SELECT uuid, \
        \       (ai_assistant ->> 'enabled')::bool, \
        \       true, \
        \       created_at, \
        \       updated_at \
        \FROM tenant_config;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createTcMailTable dbPool = do
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
        \); \
        \INSERT INTO config_mail (tenant_uuid, config_uuid, created_at, updated_at) \
        \SELECT uuid, \
        \       mail_config_uuid, \
        \       created_at, \
        \       updated_at \
        \FROM tenant_config;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
createGravatarFunction dbPool = do
  let sql =
        "CREATE OR REPLACE FUNCTION gravatar_hash(email VARCHAR) RETURNS VARCHAR \
        \    language plpgsql \
        \as \
        \$$ \
        \DECLARE \
        \    hash VARCHAR; \
        \BEGIN \
        \    SELECT md5(lower(trim(email))) \
        \    INTO hash; \
        \    RETURN hash; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

recreateGetBranchForkOfPackageIdFunction dbPool = do
  let sql =
        "DROP FUNCTION get_branch_fork_of_package_id(tenant_config, package, branch); \
        \CREATE or REPLACE FUNCTION get_branch_fork_of_package_id(config_organization config_organization, \
        \                                                         previous_pkg package, \
        \                                                         branch branch) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    fork_of_package_id varchar; \
        \BEGIN \
        \    SELECT CASE \
        \               WHEN branch.previous_package_id IS NULL THEN NULL \
        \               WHEN previous_pkg.organization_id = config_organization.organization_id AND \
        \                    previous_pkg.km_id = branch.km_id THEN previous_pkg.fork_of_package_id \
        \               WHEN True THEN branch.previous_package_id END as fork_of_package_id \
        \    INTO fork_of_package_id; \
        \    RETURN fork_of_package_id; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

dropTenantConfigTable dbPool = do
  let sql = "DROP TABLE tenant_config;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
