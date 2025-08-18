module Registry.Database.Migration.Production.Migration_0014_documentTemplateMetamodel.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 14, mmName = "DT metamodel", mmDescription = "Document template metamodel as tuple"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  createSemVer2TupleType dbPool
  changeMetamodelVersionColumnTypeInDocumentTemplate dbPool
  createDocumentTemplateFormatTable dbPool
  createDocumentTemplateFormatStepTable dbPool
  dropDocumentTemplateFormatColumn dbPool
  refactorAuditStatisticsColumn dbPool

createSemVer2TupleType dbPool = do
  let sql =
        "CREATE TYPE sem_ver_2_tuple AS ( \
        \    major INT, \
        \    minor INT \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

changeMetamodelVersionColumnTypeInDocumentTemplate dbPool = do
  let sql =
        "ALTER TABLE document_template \
        \ALTER COLUMN metamodel_version \
        \    TYPE sem_ver_2_tuple \
        \    USING ROW (metamodel_version, 0);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createDocumentTemplateFormatTable dbPool = do
  let sql =
        "CREATE TABLE document_template_format \
        \( \
        \    document_template_id varchar     NOT NULL, \
        \    uuid                 uuid        NOT NULL, \
        \    name                 varchar     NOT NULL, \
        \    icon                 varchar     NOT NULL, \
        \    tenant_uuid          uuid        NOT NULL, \
        \    created_at           timestamptz NOT NULL, \
        \    updated_at           timestamptz NOT NULL, \
        \    CONSTRAINT document_template_format_pk PRIMARY KEY (document_template_id, uuid), \
        \    CONSTRAINT document_template_format_document_template_id_fk FOREIGN KEY (document_template_id) REFERENCES document_template (id) ON DELETE CASCADE \
        \); \
        \INSERT INTO document_template_format \
        \SELECT document_template_id, \
        \       (format ->> 'uuid')::uuid AS uuid, \
        \       format ->> 'name'         AS name, \
        \       format ->> 'icon'         AS icon, \
        \       tenant_uuid, \
        \       created_at, \
        \       updated_at \
        \FROM (SELECT id                            AS document_template_id, \
        \             tenant_uuid                   AS tenant_uuid, \
        \             jsonb_array_elements(formats) AS format, \
        \             created_at, \
        \             updated_at \
        \      FROM document_template) nested;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createDocumentTemplateFormatStepTable dbPool = do
  let sql =
        "CREATE TABLE document_template_format_step \
        \( \
        \    document_template_id varchar     NOT NULL, \
        \    format_uuid          uuid        NOT NULL, \
        \    position             int         NOT NULL, \
        \    name                 varchar     NOT NULL, \
        \    options              jsonb       NOT NULL, \
        \    tenant_uuid          uuid        NOT NULL, \
        \    created_at           timestamptz NOT NULL, \
        \    updated_at           timestamptz NOT NULL, \
        \    CONSTRAINT document_template_format_step_pk PRIMARY KEY (document_template_id, format_uuid, position), \
        \    CONSTRAINT document_template_format_step_document_template_id_fk FOREIGN KEY (document_template_id) REFERENCES document_template (id) ON DELETE CASCADE, \
        \    CONSTRAINT document_template_format_step_format_uuid_fk FOREIGN KEY (document_template_id, format_uuid) REFERENCES document_template_format (document_template_id, uuid) ON DELETE CASCADE \
        \); \
        \INSERT INTO document_template_format_step \
        \WITH tmp_table AS (SELECT document_template_id, \
        \                          (format ->> 'uuid')::uuid   AS format_uuid, \
        \                          (format ->> 'steps')::jsonb AS steps, \
        \                          tenant_uuid, \
        \                          created_at, \
        \                          updated_at \
        \                   FROM (SELECT id                            AS document_template_id, \
        \                                tenant_uuid                   AS tenant_uuid, \
        \                                jsonb_array_elements(formats) AS format, \
        \                                created_at, \
        \                                updated_at \
        \                         FROM document_template) nested)\
        \SELECT tmp_table.document_template_id, \
        \       tmp_table.format_uuid, \
        \       e.ordinality - 1                   AS position, \
        \       e.value ->> 'name'             AS name, \
        \       (e.value ->> 'options')::jsonb AS options, \
        \       tmp_table.tenant_uuid, \
        \       tmp_table.created_at, \
        \       tmp_table.updated_at \
        \FROM tmp_table \
        \CROSS JOIN LATERAL jsonb_array_elements(tmp_table.steps) WITH ORDINALITY AS e(value, ordinality) \
        \ORDER BY tmp_table.document_template_id, tmp_table.format_uuid, position;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

dropDocumentTemplateFormatColumn dbPool = do
  let sql = "ALTER TABLE document_template DROP COLUMN formats;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

refactorAuditStatisticsColumn dbPool = do
  let sql =
        "ALTER TABLE audit RENAME COLUMN package_id TO package_id_old; \
        \ \
        \ALTER TABLE audit ADD COLUMN user_count int; \
        \ALTER TABLE audit ADD COLUMN package_count int; \
        \ALTER TABLE audit ADD COLUMN branch_count int; \
        \ALTER TABLE audit ADD COLUMN questionnaire_count int; \
        \ALTER TABLE audit ADD COLUMN document_template_count int; \
        \ALTER TABLE audit ADD COLUMN document_count int; \
        \ALTER TABLE audit ADD COLUMN package_id varchar; \
        \ALTER TABLE audit ADD COLUMN document_template_id varchar; \
        \ALTER TABLE audit ADD COLUMN locale_id varchar; \
        \ \
        \UPDATE audit \
        \SET document_count          = (instance_statistics ->> 'docCount')::int, \
        \    package_count           = (instance_statistics ->> 'pkgCount')::int, \
        \    questionnaire_count     = (instance_statistics ->> 'qtnCount')::int, \
        \    document_template_count = (instance_statistics ->> 'tmlCount')::int, \
        \    user_count              = (instance_statistics ->> 'userCount')::int, \
        \    branch_count            = (instance_statistics ->> 'branchCount')::int \
        \WHERE type = 'ListPackagesAuditEntry'; \
        \UPDATE audit SET package_id = package_id_old WHERE type = 'GetPackageBundleAuditEntry'; \
        \UPDATE audit SET document_template_id = package_id_old WHERE type = 'GetDocumentTemplateBundleAuditEntry'; \
        \UPDATE audit SET locale_id = package_id_old WHERE type = 'GetLocaleBundleAuditEntry'; \
        \ \
        \ALTER TABLE audit DROP COLUMN instance_statistics; \
        \ALTER TABLE audit DROP COLUMN package_id_old;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
