module Wizard.Database.Migration.Production.Migration_0058_documentTemplateMetamodel.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 58, mmName = "DT metamodel + new API integration", mmDescription = "Document template metamodel as tuple, new API integration and KM secrets"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  upgradeKmMetamodel dbPool
  updateMetamodelVersionForDocumentTemplateEditor dbPool
  createSemVer2TupleType dbPool
  changeMetamodelVersionColumnTypeInDocumentTemplate dbPool
  addValueRawColumnToQuestionnaireEventTable dbPool
  createKnowledgeModelSecretTable dbPool
  createDocumentTemplateFormatTable dbPool
  createDocumentTemplateFormatStepTable dbPool
  dropDocumentTemplateFormatColumn dbPool
  addConstraintToQuestionnaire dbPool
  addConstraintToDocument dbPool
  addConstraintToConfigSubmissionServiceSupportFormat dbPool
  createUserEntitySubmissionPropTable dbPool
  dropUserSubmissionPropsColumn dbPool
  changeUserSourceFromJsonToArray dbPool
  changeQuestionnaireSelectedQuestionTagUuidsFromJsonToArray dbPool
  changeQuestionnaireMigrationResolvedQuestionUuidsFromJsonToArray dbPool

upgradeKmMetamodel dbPool = do
  let sql =
        "TRUNCATE knowledge_model_cache; \
        \UPDATE tenant SET state = 'PendingHousekeepingTenantState';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

updateMetamodelVersionForDocumentTemplateEditor dbPool = do
  let sql = "UPDATE document_template SET metamodel_version = 17 WHERE phase = 'DraftDocumentTemplatePhase';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

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

addValueRawColumnToQuestionnaireEventTable dbPool = do
  let sql = "ALTER TABLE questionnaire_event ADD value_raw jsonb;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createKnowledgeModelSecretTable dbPool = do
  let sql =
        "CREATE TABLE knowledge_model_secret \
        \( \
        \    uuid        uuid        NOT NULL, \
        \    name        varchar     NOT NULL, \
        \    value       varchar     NOT NULL, \
        \    tenant_uuid uuid        NOT NULL, \
        \    created_at  timestamptz NOT NULL, \
        \    updated_at  timestamptz NOT NULL, \
        \    CONSTRAINT knowledge_model_secret_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT knowledge_model_secret_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
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
        \    CONSTRAINT document_template_format_pk PRIMARY KEY (uuid, document_template_id, tenant_uuid), \
        \    CONSTRAINT document_template_format_document_template_id_fk FOREIGN KEY (document_template_id, tenant_uuid) REFERENCES document_template (id, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT document_template_format_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
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
        \    CONSTRAINT document_template_format_step_pk PRIMARY KEY (document_template_id, format_uuid, position, tenant_uuid), \
        \    CONSTRAINT document_template_format_step_document_template_id_fk FOREIGN KEY (document_template_id, tenant_uuid) REFERENCES document_template (id, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT document_template_format_step_format_uuid_fk FOREIGN KEY (document_template_id, format_uuid, tenant_uuid) REFERENCES document_template_format (document_template_id, uuid, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT document_template_format_step_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
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

addConstraintToQuestionnaire dbPool = do
  let sql =
        "ALTER TABLE questionnaire \
        \    ADD CONSTRAINT questionnaire_format_uuid_fk FOREIGN KEY (document_template_id, format_uuid, tenant_uuid) REFERENCES document_template_format (document_template_id, uuid, tenant_uuid)"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addConstraintToDocument dbPool = do
  let sql =
        "ALTER TABLE document \
        \    ADD CONSTRAINT document_format_uuid_fk FOREIGN KEY (document_template_id, format_uuid, tenant_uuid) REFERENCES document_template_format (document_template_id, uuid, tenant_uuid)"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addConstraintToConfigSubmissionServiceSupportFormat dbPool = do
  let sql =
        "ALTER TABLE config_submission_service_supported_format \
        \    ADD CONSTRAINT config_submission_service_supported_format_format_uuid_fk FOREIGN KEY (document_template_id, format_uuid, tenant_uuid) REFERENCES document_template_format (document_template_id, uuid, tenant_uuid) ON DELETE CASCADE"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createUserEntitySubmissionPropTable dbPool = do
  let sql =
        "CREATE TABLE user_entity_submission_prop \
        \( \
        \    user_uuid   uuid        NOT NULL, \
        \    service_id  varchar     NOT NULL, \
        \    values      jsonb       NOT NULL, \
        \    tenant_uuid uuid        NOT NULL, \
        \    created_at  timestamptz NOT NULL, \
        \    updated_at  timestamptz NOT NULL, \
        \    CONSTRAINT user_entity_submission_prop_pk PRIMARY KEY (user_uuid, service_id, tenant_uuid), \
        \    CONSTRAINT user_entity_submission_prop_user_uuid_fk FOREIGN KEY (user_uuid, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT user_entity_submission_prop_service_id_fk FOREIGN KEY (tenant_uuid, service_id) REFERENCES config_submission_service (tenant_uuid, id) ON DELETE CASCADE, \
        \    CONSTRAINT user_entity_submission_prop_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \INSERT INTO user_entity_submission_prop (user_uuid, service_id, values, tenant_uuid, created_at, updated_at) \
        \SELECT user_uuid, \
        \       prop ->> 'id'              AS service_id, \
        \       (prop ->> 'values')::jsonb AS values, \
        \       tenant_uuid, \
        \       created_at, \
        \       updated_at \
        \FROM (SELECT uuid                                    AS user_uuid, \
        \             jsonb_array_elements(submissions_props) AS prop, \
        \             tenant_uuid                             AS tenant_uuid, \
        \             created_at                              AS created_at, \
        \             updated_at                              AS updated_at \
        \      FROM user_entity) nested;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

dropUserSubmissionPropsColumn dbPool = do
  let sql = "ALTER TABLE user_entity DROP COLUMN submissions_props;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

changeUserSourceFromJsonToArray dbPool = do
  let sql = "ALTER TABLE user_entity ALTER COLUMN sources TYPE varchar[] USING string_to_array(replace(replace(replace(replace(sources::text, '\"', ''), ' ', ''), '[', ''), ']', ''), ',')::varchar[]"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

changeQuestionnaireSelectedQuestionTagUuidsFromJsonToArray dbPool = do
  let sql = "ALTER TABLE questionnaire ALTER COLUMN selected_question_tag_uuids TYPE uuid[] USING string_to_array(replace(replace(replace(replace(selected_question_tag_uuids::text, '\"', ''), ' ', ''), '[', ''), ']', ''), ',')::uuid[]"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

changeQuestionnaireMigrationResolvedQuestionUuidsFromJsonToArray dbPool = do
  let sql = "ALTER TABLE questionnaire_migration ALTER COLUMN resolved_question_uuids TYPE uuid[] USING string_to_array(replace(replace(replace(replace(resolved_question_uuids::text, '\"', ''), ' ', ''), '[', ''), ']', ''), ',')::uuid[]"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
