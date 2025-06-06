module Wizard.Database.Migration.Production.Migration_0057_tenantConfigSubmission.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 57, mmName = "Refactor tenant_config submission", mmDescription = "Refactor tenant_config submission"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
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
        \FROM tenant_config; \
        \ \
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
        \           jsonb_array_elements((submission ->> 'services')::jsonb) with ordinality as t(json_content, idx)) service; \
        \ \
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
        \INSERT INTO config_submission_service_request_header (tenant_uuid, service_id, name, value) \
        \SELECT service.tenant_uuid           AS tenant_uuid, \
        \       service.json_content ->> 'id' AS service_id, \
        \       key                           AS name, \
        \       value                         AS value \
        \FROM (SELECT uuid as tenant_uuid, \
        \             json_content \
        \      FROM tenant_config, \
        \           jsonb_array_elements((submission ->> 'services')::jsonb) json_content) service, jsonb_each_text((((service.json_content ->> 'request')::jsonb) ->> 'headers')::jsonb); \
        \ \
        \CREATE TABLE config_submission_service_supported_format \
        \( \
        \    tenant_uuid          uuid    NOT NULL, \
        \    service_id           varchar NOT NULL, \
        \    document_template_id varchar NOT NULL, \
        \    format_uuid          uuid    NOT NULL, \
        \    CONSTRAINT config_submission_service_supported_format_pk PRIMARY KEY (tenant_uuid, service_id, document_template_id, format_uuid), \
        \    CONSTRAINT config_submission_service_supported_format_service_id_fk FOREIGN KEY (service_id, tenant_uuid) REFERENCES config_submission_service (id, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT config_submission_service_supported_format_document_template_id_fk FOREIGN KEY (document_template_id, tenant_uuid) REFERENCES document_template (id, tenant_uuid) ON DELETE CASCADE, \
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
        \           jsonb_array_elements((submission ->> 'services')::jsonb) json_content) service, jsonb_array_elements(((service.json_content ->> 'supportedFormats')::jsonb)); \
        \ \
        \ALTER TABLE tenant_config \
        \    DROP COLUMN submission;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
