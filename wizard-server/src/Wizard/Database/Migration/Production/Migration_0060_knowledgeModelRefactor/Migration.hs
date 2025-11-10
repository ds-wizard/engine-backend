module Wizard.Database.Migration.Production.Migration_0060_knowledgeModelRefactor.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 60, mmName = "Refactor knowledge model", mmDescription = "Refactor knowledge model to use new schema"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  -- KM EDITOR
  renameTableBranchToKnowledgeModelEditor dbPool
  addNewColumnsFromBranchDataToKmEditor dbPool
  copyDataFromBranchDataToKmEditor dbPool
  addConstraintsToKmEditor dbPool
  createKmEditorEventTable dbPool
  createKmEditorEventMappingTable dbPool
  fillKmEditorEventUuidMappingTable dbPool
  fixKmEditorEventUuidMappingDuplicateUuids dbPool
  fixKmEditorEventUuidMappingCreatedAtTimestamps dbPool
  moveDataToKmEditorEvents dbPool
  createKnowledgeModelEditorReplyTypeEnum dbPool
  createKnowledgeModelEditorReplyTable dbPool
  changeBranchDataRepliesToJsonb dbPool
  moveStringReplyToKmEditorReply dbPool
  moveStringArrayReplyToKmEditorReply dbPool
  moveIntegrationReplyToKmEditorReply dbPool
  -- KM PACKAGE
  renameTablePackageToKnowledgeModelPackage dbPool
  createKnowledgeModelPackageEventTable dbPool
  createKnowledgeModelPackageEventMappingTable dbPool
  fillKmPackageEventUuidMappingTable dbPool
  fixKmPackageEventUuidMappingDuplicateUuids dbPool
  fixKmPackageEventUuidMappingCreatedAtTimestamps dbPool
  moveDataToKmPackageEvents dbPool
  -- TEST
  testKmEventCount dbPool
  testKmEventOrder dbPool
  testKmReplyCount dbPool
  testKmReplyValues dbPool
  testKmReplyValueOrder dbPool
  testKmPackageEventCount dbPool
  testKmPackageEventOrder dbPool
  -- CLEANUP
  dropEventsColumnFromKnowledgeModelPackage dbPool
  dropGetBranchStateFunction dbPool
  dropBranchDataTable dbPool
  dropKmEditorEventUuidMappingTable dbPool
  dropPackageEventUuidMappingTable dbPool
  -- OTHER
  renameToKnowledgeModelEditorsInTenantLimitBundle dbPool
  createPersistentCommandFromDocumentDeleteFunction dbPool
  createTriggerOnAfterDocumentDelete dbPool
  deleteOrphanedSubmissions dbPool
  createForeignKeyConstraintFromSubmissionToConfigSubmissionService dbPool
  renameToRegistryKnowledgeModelPackage dbPool
  renameToBranchInKnowledgeModelMigration dbPool
  renamePackageConstraintsAndIndexToKnowledgeModelPackage dbPool
  renameBranchConstraintsAndIndexToKnowledgeModelEditor dbPool
  renameDocumentTemplateForeignKeys dbPool
  renameEventTypeToQuestionnaireEventType dbPool
  renameGetKnowledgeModelEditorStateFunction dbPool
  renameGetKnowledgeModelEditorForkOfPackageIdFunction dbPool
  renameKnowledgeModelPackageFunctions dbPool
  updateKnowledgeModelPackagePhase dbPool
  renamePackageIdAtFeedback dbPool
  renamePackageIdAtQuestionnaire dbPool
  addOnDeleteCascadeToPackageIdInKnowledgeModelCache dbPool
  upgradeKmMetamodel dbPool

-- ------------------------------------------------------------------------------------------------------------------------
-- 1.1) RENAME TABLE branch TO knowledge_model_editor
-- ------------------------------------------------------------------------------------------------------------------------
renameTableBranchToKnowledgeModelEditor dbPool = do
  let sql = "ALTER TABLE branch RENAME TO knowledge_model_editor;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 1.2) ADD NEW COLUMNS FROM branch_data TO knowledge_model_editor
-- ------------------------------------------------------------------------------------------------------------------------
addNewColumnsFromBranchDataToKmEditor dbPool = do
  let sql =
        "ALTER TABLE knowledge_model_editor \
        \    ADD COLUMN metamodel_version INTEGER, \
        \    ADD COLUMN squashed BOOLEAN;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 1.3) COPY DATA FROM branch_data TO knowledge_model_editor
-- NOTE: take created_at from branch -> no migration needed, take updated_at from branch_data? -> use most recent one
-- ------------------------------------------------------------------------------------------------------------------------
copyDataFromBranchDataToKmEditor dbPool = do
  let sql =
        "UPDATE knowledge_model_editor \
        \SET metamodel_version = branch_data.metamodel_version, \
        \    squashed          = branch_data.squashed \
        \FROM branch_data \
        \WHERE branch_data.branch_uuid = knowledge_model_editor.uuid \
        \  AND branch_data.tenant_uuid = knowledge_model_editor.tenant_uuid; \
        \ \
        \UPDATE knowledge_model_editor \
        \SET updated_at = GREATEST(knowledge_model_editor.updated_at, branch_data.updated_at) \
        \FROM branch_data \
        \WHERE branch_data.branch_uuid = knowledge_model_editor.uuid \
        \  AND branch_data.tenant_uuid = knowledge_model_editor.tenant_uuid \
        \  AND knowledge_model_editor.updated_at != GREATEST(knowledge_model_editor.updated_at, branch_data.updated_at);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 1.4) ADD CONSTRAINTS
-- ------------------------------------------------------------------------------------------------------------------------
addConstraintsToKmEditor dbPool = do
  let sql =
        "ALTER TABLE knowledge_model_editor \
        \    ALTER COLUMN metamodel_version SET NOT NULL, \
        \    ALTER COLUMN squashed SET NOT NULL, \
        \    ALTER COLUMN squashed SET DEFAULT FALSE;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 1.5) CREATE KM EDITOR EVENT TABLE
-- ------------------------------------------------------------------------------------------------------------------------
createKmEditorEventTable dbPool = do
  let sql =
        "CREATE TABLE IF NOT EXISTS knowledge_model_editor_event \
        \( \
        \    uuid        UUID        NOT NULL, \
        \    parent_uuid UUID        NOT NULL, \
        \    entity_uuid UUID        NOT NULL, \
        \    content     JSONB       NOT NULL, \
        \    editor_uuid UUID        NOT NULL, \
        \    tenant_uuid UUID        NOT NULL, \
        \    created_at  TIMESTAMPTZ NOT NULL, \
        \    CONSTRAINT knowledge_model_editor_event_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT knowledge_model_editor_event_editor_uuid_fk FOREIGN KEY (editor_uuid, tenant_uuid) REFERENCES knowledge_model_editor (uuid, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT knowledge_model_editor_event_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 1.6) CREATE KM EDITOR EVENT MAPPING TABLE
-- ------------------------------------------------------------------------------------------------------------------------
createKmEditorEventMappingTable dbPool = do
  let sql =
        "CREATE TEMP TABLE km_editor_event_uuid_mapping \
        \( \
        \    tenant_uuid    UUID        NOT NULL, \
        \    editor_uuid    UUID        NOT NULL, \
        \    old_uuid       UUID        NOT NULL, \
        \    new_uuid       UUID        NOT NULL, \
        \    old_created_at TIMESTAMPTZ NOT NULL, \
        \    new_created_at TIMESTAMPTZ NOT NULL, \
        \    ord            INT         NOT NULL, \
        \    CONSTRAINT km_editor_event_uuid_mapping_pk PRIMARY KEY (tenant_uuid, editor_uuid, old_uuid, ord) \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- FILL IN ORIGINAL VALUES
fillKmEditorEventUuidMappingTable dbPool = do
  let sql =
        "INSERT INTO km_editor_event_uuid_mapping (tenant_uuid, \
        \                                          editor_uuid, \
        \                                          old_uuid, \
        \                                          new_uuid, \
        \                                          old_created_at, \
        \                                          new_created_at, \
        \                                          ord) \
        \SELECT b.tenant_uuid, \
        \       b.branch_uuid                      AS editor_uuid, \
        \       (e.e ->> 'uuid')::uuid             AS old_uuid, \
        \       (e.e ->> 'uuid')::uuid             AS new_uuid, \
        \       (e.e ->> 'createdAt')::timestamptz AS old_created_at, \
        \       (e.e ->> 'createdAt')::timestamptz AS new_created_at, \
        \       ord \
        \FROM branch_data b, \
        \     jsonb_array_elements(b.events) WITH ORDINALITY AS e(e, ord) \
        \WHERE NOT jsonb_typeof(e.e) = 'null';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- FIX DUPLICATE UUIDS
fixKmEditorEventUuidMappingDuplicateUuids dbPool = do
  let sql =
        "WITH numbered AS (SELECT tenant_uuid, \
        \                         editor_uuid, \
        \                         old_uuid, \
        \                         ord, \
        \                         ROW_NUMBER() OVER ( \
        \                             PARTITION BY tenant_uuid, editor_uuid, old_uuid \
        \                             ORDER BY ord \
        \                             ) AS rn \
        \                  FROM km_editor_event_uuid_mapping) \
        \UPDATE km_editor_event_uuid_mapping m \
        \SET new_uuid = gen_random_uuid() \
        \FROM numbered n \
        \WHERE m.tenant_uuid = n.tenant_uuid \
        \  AND m.editor_uuid = n.editor_uuid \
        \  AND m.old_uuid = n.old_uuid \
        \  AND m.ord = n.ord \
        \  AND n.rn > 1;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- FIX CREATED_AT TIMESTAMPS
fixKmEditorEventUuidMappingCreatedAtTimestamps dbPool = do
  let sql =
        "WITH lagged AS (SELECT editor_uuid, \
        \                       tenant_uuid, \
        \                       old_created_at, \
        \                       LAG(old_created_at) OVER (PARTITION BY editor_uuid, tenant_uuid ORDER BY ord) AS prev_created_at \
        \                FROM km_editor_event_uuid_mapping), \
        \     incorrect AS (SELECT DISTINCT editor_uuid, tenant_uuid \
        \                   FROM lagged \
        \                   WHERE prev_created_at IS NOT NULL \
        \                     AND old_created_at <= prev_created_at), \
        \     first_ts AS (SELECT editor_uuid, tenant_uuid, MIN(old_created_at) AS first_created_at \
        \                  FROM km_editor_event_uuid_mapping \
        \                  WHERE (editor_uuid, tenant_uuid) IN (SELECT editor_uuid, tenant_uuid FROM incorrect) \
        \                  GROUP BY editor_uuid, tenant_uuid), \
        \     updated AS (SELECT m.tenant_uuid, \
        \                        m.editor_uuid, \
        \                        m.ord, \
        \                        m.old_created_at, \
        \                        first_ts.first_created_at + (m.ord * interval '1 second') AS new_created_at \
        \                 FROM km_editor_event_uuid_mapping m \
        \                      JOIN first_ts \
        \                           ON m.editor_uuid = first_ts.editor_uuid \
        \                               AND m.tenant_uuid = first_ts.tenant_uuid \
        \                 WHERE (m.editor_uuid, m.tenant_uuid) IN (SELECT editor_uuid, tenant_uuid FROM incorrect)) \
        \UPDATE km_editor_event_uuid_mapping m \
        \SET new_created_at = u.new_created_at \
        \FROM updated u \
        \WHERE m.editor_uuid = u.editor_uuid \
        \  AND m.tenant_uuid = u.tenant_uuid \
        \  AND m.ord = u.ord;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 1.7) MOVE DATA TO KM_EDITOR_EVENTS
-- ------------------------------------------------------------------------------------------------------------------------
moveDataToKmEditorEvents dbPool = do
  let sql =
        "INSERT INTO knowledge_model_editor_event (uuid, parent_uuid, entity_uuid, created_at, \
        \                                          editor_uuid, tenant_uuid, content) \
        \SELECT km_editor_event_uuid_mapping.new_uuid                  AS uuid, \
        \       (e.e ->> 'parentUuid')::uuid                           AS parent_uuid, \
        \       (e.e ->> 'entityUuid')::uuid                           AS entity_uuid, \
        \       km_editor_event_uuid_mapping.new_created_at            AS created_at, \
        \       t.branch_uuid                                          AS editor_uuid, \
        \       t.tenant_uuid                                          AS tenant_uuid, \
        \       e - 'uuid' - 'parentUuid' - 'entityUuid' - 'createdAt' AS content \
        \FROM branch_data AS t \
        \     LEFT JOIN LATERAL jsonb_array_elements(events) WITH ORDINALITY AS e(e, ord) ON TRUE \
        \     LEFT JOIN km_editor_event_uuid_mapping \
        \               ON km_editor_event_uuid_mapping.tenant_uuid = t.tenant_uuid AND \
        \                  km_editor_event_uuid_mapping.editor_uuid = t.branch_uuid AND \
        \                  km_editor_event_uuid_mapping.old_uuid = (e.e ->> 'uuid')::uuid AND \
        \                  km_editor_event_uuid_mapping.ord = e.ord \
        \WHERE NOT jsonb_typeof(e.e) = 'null';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 1.8) KM_EDITOR_EVENTS_REPLY
-- ------------------------------------------------------------------------------------------------------------------------
-- CREATE ENUM
createKnowledgeModelEditorReplyTypeEnum dbPool = do
  let sql =
        "CREATE TYPE knowledge_model_editor_reply_type AS ENUM ( \
        \    'StringReply', 'AnswerReply', 'ItemSelectReply', 'FileReply', \
        \    'IntegrationReply', 'MultiChoiceReply', 'ItemListReply' \
        \    );"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- CREATE TABLE
createKnowledgeModelEditorReplyTable dbPool = do
  let sql =
        "CREATE TABLE IF NOT EXISTS knowledge_model_editor_reply \
        \( \
        \    path        text                              NOT NULL, \
        \    value_type  knowledge_model_editor_reply_type NOT NULL, \
        \    value       text[], \
        \    value_id    text, \
        \    value_raw   jsonb, \
        \    editor_uuid uuid                              NOT NULL, \
        \    created_by  jsonb, \
        \    tenant_uuid uuid                              NOT NULL, \
        \    created_at  timestamptz                       NOT NULL, \
        \    CONSTRAINT knowledge_model_editor_reply_pk PRIMARY KEY (path, editor_uuid, tenant_uuid), \
        \    CONSTRAINT knowledge_model_editor_reply_editor_uuid FOREIGN KEY (editor_uuid, tenant_uuid) REFERENCES knowledge_model_editor (uuid, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT knowledge_model_editor_reply_tenant_uuid FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 1.9) MOVE DATA TO KM_EDITOR_EVENTS_REPLY
-- ------------------------------------------------------------------------------------------------------------------------
-- json to jsonb to be able to use group by
changeBranchDataRepliesToJsonb dbPool = do
  let sql =
        "ALTER TABLE branch_data \
        \    ALTER COLUMN replies \
        \        SET DATA TYPE jsonb \
        \        USING replies::jsonb;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- string
-- StringReply, AnswerReply, ItemSelectReply, FileReply
moveStringReplyToKmEditorReply dbPool = do
  let sql =
        "INSERT INTO knowledge_model_editor_reply \
        \(path, value_type, value, editor_uuid, created_by, tenant_uuid, created_at) \
        \SELECT kv.key                                                              AS path, \
        \       (kv.value -> 'value' ->> 'type')::knowledge_model_editor_reply_type AS value_type, \
        \       CASE \
        \           WHEN kv.value -> 'value' ->> 'value' = '' THEN '{\"\"}'::text[] \
        \           WHEN kv.value -> 'value' ->> 'value' IS NULL THEN NULL \
        \           ELSE array_agg(kv.value -> 'value' ->> 'value') \
        \           END                                                             AS value, \
        \       t.branch_uuid                                                       AS editor_uuid, \
        \       kv.value -> 'createdBy'                                             AS created_by, \
        \       t.tenant_uuid                                                       AS tenant_uuid, \
        \       (kv.value ->> 'createdAt')::timestamptz                             AS created_at \
        \FROM branch_data AS t \
        \     CROSS JOIN LATERAL jsonb_each( \
        \        CASE \
        \            WHEN t.replies IS NULL THEN '{}'::jsonb \
        \            WHEN jsonb_typeof(t.replies) = 'object' THEN t.replies \
        \            ELSE '{}'::jsonb \
        \            END \
        \                        ) AS kv(key, value) \
        \WHERE kv.value -> 'value' ->> 'type' IN ('StringReply', 'AnswerReply', 'ItemSelectReply', 'FileReply') \
        \GROUP BY kv.value, kv.key, t.tenant_uuid, t.branch_uuid;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- array of strings
-- MultiChoiceReply, ItemListReply
moveStringArrayReplyToKmEditorReply dbPool = do
  let sql =
        "INSERT INTO knowledge_model_editor_reply \
        \(path, value_type, value, editor_uuid, created_by, tenant_uuid, created_at) \
        \SELECT kv.key                                                              AS path, \
        \       (kv.value -> 'value' ->> 'type')::knowledge_model_editor_reply_type AS value_type, \
        \       CASE \
        \           WHEN kv.value -> 'value' ->> 'value' = '' THEN '{\"\"}'::text[] \
        \           WHEN kv.value -> 'value' ->> 'value' IS NULL THEN NULL \
        \           ELSE array_agg(v ORDER BY o) \
        \           END                                                             AS value, \
        \       t.branch_uuid                                                       AS editor_uuid, \
        \       kv.value -> 'createdBy'                                             AS created_by, \
        \       t.tenant_uuid                                                       AS tenant_uuid, \
        \       (kv.value ->> 'createdAt')::timestamptz                             AS created_at \
        \FROM branch_data AS t \
        \     CROSS JOIN LATERAL jsonb_each( \
        \        CASE \
        \            WHEN t.replies IS NULL THEN '{}'::jsonb \
        \            WHEN jsonb_typeof(t.replies) = 'object' THEN t.replies \
        \            ELSE '{}'::jsonb \
        \            END \
        \                        ) AS kv(key, value) \
        \     LEFT JOIN LATERAL jsonb_array_elements_text(kv.value -> 'value' -> 'value') WITH ORDINALITY AS values(v, o) ON TRUE \
        \WHERE kv.value -> 'value' ->> 'type' IN ('MultiChoiceReply', 'ItemListReply') \
        \GROUP BY kv.key, kv.value, t.tenant_uuid, t.branch_uuid;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- integration types
-- object
-- IntegrationReply: PlainType, IntegrationLegacyType, IntegrationType
moveIntegrationReplyToKmEditorReply dbPool = do
  let sql =
        "INSERT INTO knowledge_model_editor_reply \
        \(path, value_type, value, value_id, value_raw, editor_uuid, created_by, tenant_uuid, created_at) \
        \SELECT kv.key                                                              AS path, \
        \       (kv.value -> 'value' ->> 'type')::knowledge_model_editor_reply_type AS value_type, \
        \       CASE \
        \           WHEN kv.value -> 'value' -> 'value' ->> 'value' = '' THEN '{\"\"}'::text[] \
        \           WHEN kv.value -> 'value' -> 'value' ->> 'value' IS NULL THEN NULL \
        \           ELSE array_agg(kv.value -> 'value' -> 'value' ->> 'value') \
        \           END                                                             AS value, \
        \       CASE \
        \           WHEN kv.value -> 'value' -> 'value' ->> 'type' = 'PlainType' THEN NULL \
        \           WHEN kv.value -> 'value' -> 'value' ->> 'type' = 'IntegrationLegacyType' \
        \               THEN kv.value -> 'value' -> 'value' ->> 'id' \
        \           WHEN kv.value -> 'value' -> 'value' ->> 'type' = 'IntegrationType' THEN NULL \
        \           END                                                             AS value_id, \
        \       CASE \
        \           WHEN kv.value -> 'value' -> 'value' ->> 'type' = 'PlainType' THEN NULL \
        \           WHEN kv.value -> 'value' -> 'value' ->> 'type' = 'IntegrationLegacyType' THEN NULL \
        \           WHEN kv.value -> 'value' -> 'value' ->> 'type' = 'IntegrationType' \
        \               THEN kv.value -> 'value' -> 'value' -> 'raw' \
        \           END                                                             AS value_raw, \
        \       t.branch_uuid                                                       AS editor_uuid, \
        \       kv.value -> 'createdBy'                                             AS created_by, \
        \       t.tenant_uuid                                                       AS tenant_uuid, \
        \       (kv.value ->> 'createdAt')::timestamptz                             AS created_at \
        \FROM branch_data AS t \
        \     CROSS JOIN LATERAL jsonb_each( \
        \        CASE \
        \            WHEN t.replies IS NULL THEN '{}'::jsonb \
        \            WHEN jsonb_typeof(t.replies) = 'object' THEN t.replies \
        \            ELSE '{}'::jsonb \
        \            END \
        \                        ) AS kv(key, value) \
        \WHERE kv.value -> 'value' ->> 'type' = 'IntegrationReply' \
        \GROUP BY kv.value, kv.key, t.tenant_uuid, t.branch_uuid;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 2.1) RENAME TABLE package TO knowledge_model_package
-- ------------------------------------------------------------------------------------------------------------------------
renameTablePackageToKnowledgeModelPackage dbPool = do
  let sql = "ALTER TABLE package RENAME TO knowledge_model_package;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 2.2) CREATE KM PACKAGE EVENT TABLE
-- ------------------------------------------------------------------------------------------------------------------------
createKnowledgeModelPackageEventTable dbPool = do
  let sql =
        "CREATE TABLE IF NOT EXISTS knowledge_model_package_event \
        \( \
        \    uuid        UUID        NOT NULL, \
        \    parent_uuid UUID        NOT NULL, \
        \    entity_uuid UUID        NOT NULL, \
        \    content     JSONB       NOT NULL, \
        \    package_id  varchar     NOT NULL, \
        \    tenant_uuid UUID        NOT NULL, \
        \    created_at  TIMESTAMPTZ NOT NULL, \
        \    CONSTRAINT knowledge_model_package_event_pk PRIMARY KEY (uuid, package_id, tenant_uuid), \
        \    CONSTRAINT knowledge_model_package_event_package_id_fk FOREIGN KEY (package_id, tenant_uuid) REFERENCES knowledge_model_package (id, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT knowledge_model_package_event_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 2.3) CREATE KM PACKAGE EVENT MAPPING TABLE
-- ------------------------------------------------------------------------------------------------------------------------
createKnowledgeModelPackageEventMappingTable dbPool = do
  let sql =
        "CREATE TEMP TABLE package_event_uuid_mapping \
        \( \
        \    tenant_uuid    UUID        NOT NULL, \
        \    package_id     varchar     NOT NULL, \
        \    old_uuid       UUID        NOT NULL, \
        \    new_uuid       UUID        NOT NULL, \
        \    old_created_at TIMESTAMPTZ NOT NULL, \
        \    new_created_at TIMESTAMPTZ NOT NULL, \
        \    ord            INT         NOT NULL, \
        \    CONSTRAINT package_event_uuid_mapping_pk PRIMARY KEY (tenant_uuid, package_id, old_uuid, ord) \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- FILL IN ORIGINAL VALUES
fillKmPackageEventUuidMappingTable dbPool = do
  let sql =
        "INSERT INTO package_event_uuid_mapping (tenant_uuid, \
        \                                        package_id, \
        \                                        old_uuid, \
        \                                        new_uuid, \
        \                                        old_created_at, \
        \                                        new_created_at, \
        \                                        ord) \
        \SELECT p.tenant_uuid, \
        \       p.id                               AS package_id, \
        \       (e.e ->> 'uuid')::uuid             AS old_uuid, \
        \       (e.e ->> 'uuid')::uuid             AS new_uuid, \
        \       (e.e ->> 'createdAt')::timestamptz AS old_created_at, \
        \       (e.e ->> 'createdAt')::timestamptz AS new_created_at, \
        \       ord \
        \FROM knowledge_model_package p, \
        \     jsonb_array_elements(p.events) WITH ORDINALITY AS e(e, ord) \
        \WHERE NOT jsonb_typeof(e.e) = 'null';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- FIX DUPLICATE UUIDS
fixKmPackageEventUuidMappingDuplicateUuids dbPool = do
  let sql =
        "WITH numbered AS (SELECT tenant_uuid, \
        \                         package_id, \
        \                         old_uuid, \
        \                         ord, \
        \                         ROW_NUMBER() OVER ( \
        \                             PARTITION BY tenant_uuid, package_id, old_uuid \
        \                             ORDER BY ord \
        \                             ) AS rn \
        \                  FROM package_event_uuid_mapping) \
        \UPDATE package_event_uuid_mapping m \
        \SET new_uuid = gen_random_uuid() \
        \FROM numbered n \
        \WHERE m.tenant_uuid = n.tenant_uuid \
        \  AND m.package_id = n.package_id \
        \  AND m.old_uuid = n.old_uuid \
        \  AND m.ord = n.ord \
        \  AND n.rn > 1;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- FIX CREATED_AT TIMESTAMPS
fixKmPackageEventUuidMappingCreatedAtTimestamps dbPool = do
  let sql =
        "WITH lagged AS (SELECT package_id, \
        \                       tenant_uuid, \
        \                       old_created_at, \
        \                       LAG(old_created_at) OVER (PARTITION BY package_id, tenant_uuid ORDER BY ord) AS prev_created_at \
        \                FROM package_event_uuid_mapping), \
        \     incorrect AS (SELECT DISTINCT package_id, tenant_uuid \
        \                   FROM lagged \
        \                   WHERE prev_created_at IS NOT NULL \
        \                     AND old_created_at <= prev_created_at), \
        \     first_ts AS (SELECT package_id, tenant_uuid, MIN(old_created_at) AS first_created_at \
        \                  FROM package_event_uuid_mapping \
        \                  WHERE (package_id, tenant_uuid) IN (SELECT package_id, tenant_uuid FROM incorrect) \
        \                  GROUP BY package_id, tenant_uuid), \
        \     updated AS (SELECT m.tenant_uuid, \
        \                        m.package_id, \
        \                        m.ord, \
        \                        m.old_created_at, \
        \                        first_ts.first_created_at + (m.ord * interval '1 second') AS new_created_at \
        \                 FROM package_event_uuid_mapping m \
        \                      JOIN first_ts \
        \                           ON m.package_id = first_ts.package_id \
        \                               AND m.tenant_uuid = first_ts.tenant_uuid \
        \                 WHERE (m.package_id, m.tenant_uuid) IN (SELECT package_id, tenant_uuid FROM incorrect)) \
        \UPDATE package_event_uuid_mapping m \
        \SET new_created_at = u.new_created_at \
        \FROM updated u \
        \WHERE m.package_id = u.package_id \
        \  AND m.tenant_uuid = u.tenant_uuid \
        \  AND m.ord = u.ord;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 2.4) MOVE DATA TO KM_PACKAGE_EVENTS
-- ------------------------------------------------------------------------------------------------------------------------
moveDataToKmPackageEvents dbPool = do
  let sql =
        "INSERT INTO knowledge_model_package_event (uuid, parent_uuid, entity_uuid, created_at, \
        \                                           package_id, tenant_uuid, content) \
        \SELECT new_uuid                                               AS uuid, \
        \       (e.e ->> 'parentUuid')::uuid                           AS parent_uuid, \
        \       (e.e ->> 'entityUuid')::uuid                           AS entity_uuid, \
        \       new_created_at                                         AS created_at, \
        \       t.id                                                   AS package_id, \
        \       t.tenant_uuid                                          AS tenant_uuid, \
        \       e - 'uuid' - 'parentUuid' - 'entityUuid' - 'createdAt' AS content \
        \FROM knowledge_model_package AS t \
        \     LEFT JOIN LATERAL jsonb_array_elements(events) WITH ORDINALITY AS e(e, ord) ON TRUE \
        \     LEFT JOIN package_event_uuid_mapping \
        \               ON package_event_uuid_mapping.tenant_uuid = t.tenant_uuid AND \
        \                  package_event_uuid_mapping.package_id = t.id AND \
        \                  package_event_uuid_mapping.old_uuid = (e.e ->> 'uuid')::uuid AND \
        \                  package_event_uuid_mapping.ord = e.ord \
        \WHERE NOT jsonb_typeof(e.e) = 'null';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 3.1) TEST KM EVENT COUNT
-- ------------------------------------------------------------------------------------------------------------------------
testKmEventCount dbPool = do
  let sql =
        "DO \
        \$$ \
        \    DECLARE \
        \        mismatches TEXT; \
        \    BEGIN \
        \        SELECT string_agg(COALESCE(t1.uuid::text, t2.editor_uuid::text), ', ') \
        \        INTO mismatches \
        \        FROM (SELECT branch_uuid as uuid, \
        \                     CASE \
        \                         WHEN events = '[ \
        \                           null \
        \                         ]'::jsonb THEN 0 \
        \                         ELSE COALESCE(jsonb_array_length(events), 0) \
        \                         END     AS event_count \
        \              FROM branch_data) AS t1 \
        \             FULL OUTER JOIN \
        \             (SELECT editor_uuid, COUNT(*) AS event_count \
        \              FROM knowledge_model_editor_event \
        \              GROUP BY editor_uuid) AS t2 \
        \             ON t1.uuid = t2.editor_uuid \
        \        WHERE (t1.event_count <> t2.event_count) \
        \           OR (t1.event_count IS NULL) \
        \           OR (t2.event_count IS NULL AND t1.event_count <> 0); \
        \ \
        \        IF mismatches IS NULL THEN \
        \            RAISE NOTICE 'Test PASSED: no mismatched event counts for knowledge_model_event table.'; \
        \        ELSE \
        \            RAISE EXCEPTION 'Test FAILED: mismatched UUIDs: %', mismatches; \
        \        END IF; \
        \    END; \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 3.2) TEST KM EVENT ORDER
-- ------------------------------------------------------------------------------------------------------------------------
testKmEventOrder dbPool = do
  let sql =
        "DO \
        \$$ \
        \    DECLARE \
        \        mismatches INT; \
        \    BEGIN \
        \        WITH json_events AS (SELECT b.branch_uuid           AS id, \
        \                                    b.tenant_uuid, \
        \                                    arr.ord                 AS rn, \
        \                                    (SELECT new_uuid::text \
        \                                     FROM km_editor_event_uuid_mapping m \
        \                                     WHERE b.branch_uuid = m.editor_uuid \
        \                                       AND b.tenant_uuid = m.tenant_uuid \
        \                                       AND arr.value ->> 'uuid' = m.old_uuid::text \
        \                                       AND arr.ord = m.ord) AS uuid \
        \                             FROM branch_data b \
        \                                  CROSS JOIN LATERAL jsonb_array_elements(b.events) WITH ORDINALITY arr(value, ord)), \
        \             row_events AS (SELECT e.uuid                                                                       AS id, \
        \                                   e.tenant_uuid, \
        \                                   ROW_NUMBER() OVER (PARTITION BY e.uuid, e.tenant_uuid ORDER BY e.created_at) AS rn, \
        \                                   e.uuid::text                                                                 AS uuid \
        \                            FROM knowledge_model_editor_event e) \
        \        SELECT COUNT(*) \
        \        INTO mismatches \
        \        FROM json_events j \
        \             JOIN row_events r \
        \                  ON j.id = r.id \
        \                      AND j.tenant_uuid = r.tenant_uuid \
        \                      AND j.rn = r.rn \
        \        WHERE j.uuid IS DISTINCT FROM r.uuid; \
        \ \
        \        IF mismatches > 0 THEN \
        \            RAISE EXCEPTION 'Test FAILED: % mismatches between JSON order and row order.', mismatches; \
        \        ELSE \
        \            RAISE NOTICE 'Test PASSED: all UUIDs have matching event order between JSON and row-based table.'; \
        \        END IF; \
        \    END; \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 3.3) TEST KM REPLY COUNT
-- ------------------------------------------------------------------------------------------------------------------------
testKmReplyCount dbPool = do
  let sql =
        "DO \
        \$$ \
        \    DECLARE \
        \        mismatches TEXT; \
        \    BEGIN \
        \        SELECT string_agg(COALESCE(t1.uuid::text, t2.editor_uuid::text), ', ') \
        \        INTO mismatches \
        \        FROM (SELECT branch_uuid                                                    as uuid, \
        \                     COALESCE((SELECT COUNT(*) FROM jsonb_object_keys(replies)), 0) AS reply_count \
        \              FROM branch_data) AS t1 \
        \             FULL OUTER JOIN \
        \             (SELECT editor_uuid, COUNT(*) AS reply_count \
        \              FROM knowledge_model_editor_reply \
        \              GROUP BY editor_uuid) AS t2 \
        \             ON t1.uuid = t2.editor_uuid \
        \        WHERE (t1.reply_count <> t2.reply_count) \
        \           OR (t1.reply_count IS NULL) \
        \           OR (t2.reply_count IS NULL AND t1.reply_count <> 0); \
        \ \
        \        IF mismatches IS NULL THEN \
        \            RAISE NOTICE 'Test PASSED: no mismatched reply counts for knowledge_model_reply table.'; \
        \        ELSE \
        \            RAISE EXCEPTION 'Test FAILED: mismatched UUIDs: %', mismatches; \
        \        END IF; \
        \    END; \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 3.4) TEST KM REPLY VALUES
-- ------------------------------------------------------------------------------------------------------------------------
testKmReplyValues dbPool = do
  let sql =
        "CREATE TEMP TABLE temp_value_counts \
        \( \
        \    branch_uuid          UUID, \
        \    path                 TEXT, \
        \    value_count_original INT, \
        \    value_count_editor   INT \
        \); \
        \ \
        \INSERT INTO temp_value_counts (branch_uuid, path, value_count_original, value_count_editor) \
        \SELECT COALESCE(b.branch_uuid, t2.editor_uuid), \
        \       COALESCE(t1.path, t2.path), \
        \       t1.value_count AS value_count_original, \
        \       t2.value_count AS value_count_editor \
        \FROM branch_data b \
        \     LEFT JOIN LATERAL ( \
        \    SELECT branch_uuid, \
        \           tenant_uuid, \
        \           key AS path, \
        \           SUM( \
        \                   CASE \
        \                       WHEN value -> 'value' ->> 'type' <> 'IntegrationReply' AND \
        \                            jsonb_typeof(value -> 'value' -> 'value') = 'null' THEN 0 \
        \                       WHEN value -> 'value' ->> 'type' = 'IntegrationReply' AND \
        \                            jsonb_typeof(value -> 'value' -> 'value' -> 'value') = 'null' THEN 0 \
        \                       WHEN value -> 'value' ->> 'type' IN ('MultiChoiceReply', 'ItemListReply') \
        \                           THEN jsonb_array_length(value -> 'value' -> 'value') \
        \                       WHEN value -> 'value' ->> 'type' IN \
        \                            ('StringReply', 'AnswerReply', 'ItemSelectReply', 'FileReply') THEN 1 \
        \                       WHEN value -> 'value' ->> 'type' = 'IntegrationReply' THEN 1 \
        \                       ELSE jsonb_array_length(value -> 'value' -> 'value') \
        \                       END \
        \           )   AS value_count \
        \    FROM jsonb_each(b.replies) AS t(key, value) \
        \    GROUP BY branch_uuid, tenant_uuid, t.key, t.value \
        \    ) AS t1 ON TRUE \
        \     FULL OUTER JOIN (SELECT path, tenant_uuid, editor_uuid, COALESCE(array_length(value, 1), 0) AS value_count \
        \                      FROM knowledge_model_editor_reply \
        \                      GROUP BY path, tenant_uuid, value, editor_uuid) AS t2 \
        \                     ON t1.path = t2.path AND t1.branch_uuid = t2.editor_uuid AND t1.tenant_uuid = t2.tenant_uuid; \
        \ \
        \DO \
        \$$ \
        \    DECLARE \
        \        rec RECORD; \
        \    BEGIN \
        \        FOR rec IN \
        \            SELECT * \
        \            FROM temp_value_counts \
        \            WHERE value_count_original IS DISTINCT FROM value_count_editor \
        \            LOOP \
        \                RAISE NOTICE 'Branch: %, Path: %, Original: %, Editor: %', \
        \                    rec.branch_uuid, \
        \                    rec.path, \
        \                    rec.value_count_original, \
        \                    rec.value_count_editor; \
        \            END LOOP; \
        \ \
        \        IF NOT EXISTS (SELECT 1 \
        \                       FROM temp_value_counts \
        \                       WHERE value_count_original IS DISTINCT FROM value_count_editor) THEN \
        \            RAISE NOTICE 'Test PASSED: All value counts match.'; \
        \        ELSE \
        \            RAISE EXCEPTION 'Test FAILED: Some value counts mismatch (see above notices).'; \
        \        END IF; \
        \    END; \
        \$$; \
        \ \
        \DROP TABLE temp_value_counts;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 3.5) TEST KM REPLY VALUE ORDER
-- ------------------------------------------------------------------------------------------------------------------------
testKmReplyValueOrder dbPool = do
  let sql =
        "DO \
        \$$ \
        \    DECLARE \
        \        mismatches TEXT; \
        \    BEGIN \
        \        WITH json1_elements AS (SELECT v.key AS path, \
        \                                       o, \
        \                                       val   AS value \
        \                                FROM branch_data t1 \
        \                                     JOIN LATERAL jsonb_each(t1.replies) AS v(key, value) ON TRUE \
        \                                     JOIN LATERAL jsonb_array_elements(v.value -> 'value' -> 'value') WITH ORDINALITY AS values(val, o) \
        \                                          ON TRUE \
        \                                WHERE v.value -> 'value' ->> 'type' IN ('MultiChoiceReply', 'ItemListReply')), \
        \             json2_elements AS (SELECT path, \
        \                                       o, \
        \                                       v \
        \                                FROM knowledge_model_editor_reply t2 \
        \                                     JOIN LATERAL jsonb_array_elements(to_jsonb(t2.value)) WITH ORDINALITY AS values(v, o) \
        \                                          ON TRUE \
        \                                WHERE t2.value_type IN ('MultiChoiceReply', 'ItemListReply')) \
        \        SELECT 'Path: ' || COALESCE(j1.path, j2.path) || ' Values: ' || j1.value \
        \        INTO mismatches \
        \        FROM json1_elements j1 \
        \             JOIN json2_elements j2 \
        \                  ON j1.path = j2.path \
        \                      AND j1.value = j2.v \
        \                      AND j1.o <> j2.o; \
        \ \
        \        IF mismatches IS NULL THEN \
        \            RAISE NOTICE 'Test PASSED: no mismatched reply counts for knowledge_model_reply table.'; \
        \        ELSE \
        \            RAISE EXCEPTION 'Test FAILED: mismatched: % in knowledge_model_reply table', mismatches; \
        \        END IF; \
        \    END; \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 3.6) TEST KM PACKAGE EVENT COUNT
-- ------------------------------------------------------------------------------------------------------------------------
testKmPackageEventCount dbPool = do
  let sql =
        "DO \
        \$$ \
        \    DECLARE \
        \        mismatches TEXT; \
        \    BEGIN \
        \        SELECT string_agg(COALESCE(t1.package_id::text, t2.package_id::text), ', ') \
        \        INTO mismatches \
        \        FROM (SELECT id      AS package_id, \
        \                     tenant_uuid, \
        \                     CASE \
        \                         WHEN events = '[ \
        \                           null \
        \                         ]'::jsonb THEN 0 \
        \                         ELSE COALESCE(jsonb_array_length(events), 0) \
        \                         END AS json_event_count \
        \              FROM knowledge_model_package) AS t1 \
        \             FULL OUTER JOIN \
        \             (SELECT package_id, \
        \                     tenant_uuid, \
        \                     COUNT(*) AS table_event_count \
        \              FROM knowledge_model_package_event \
        \              GROUP BY package_id, tenant_uuid) AS t2 \
        \             ON t1.package_id = t2.package_id AND t1.tenant_uuid = t2.tenant_uuid \
        \        WHERE (t1.json_event_count <> t2.table_event_count) \
        \           OR (t1.json_event_count IS NULL) \
        \           OR (t2.table_event_count IS NULL AND t1.json_event_count <> 0); \
        \ \
        \        IF mismatches IS NULL THEN \
        \            RAISE NOTICE 'Test PASSED: no mismatched event counts for knowledge_model_package_event table.'; \
        \        ELSE \
        \            RAISE EXCEPTION 'Test FAILED: mismatched UUIDs: %', mismatches; \
        \        END IF; \
        \    END; \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 3.7) TEST KM PACKAGE EVENT ORDER
-- ------------------------------------------------------------------------------------------------------------------------
testKmPackageEventOrder dbPool = do
  let sql =
        "DO \
        \$$ \
        \    DECLARE \
        \        mismatches INT; \
        \    BEGIN \
        \        WITH json_events AS (SELECT p.id, \
        \                                    p.tenant_uuid, \
        \                                    arr.ord                 AS rn, \
        \                                    (SELECT new_uuid::text \
        \                                     FROM package_event_uuid_mapping m \
        \                                     WHERE p.id = m.package_id \
        \                                       AND p.tenant_uuid = m.tenant_uuid \
        \                                       AND arr.value ->> 'uuid' = m.old_uuid::text \
        \                                       AND arr.ord = m.ord) AS uuid \
        \                             FROM knowledge_model_package p \
        \                                  CROSS JOIN LATERAL jsonb_array_elements(p.events) WITH ORDINALITY arr(value, ord)), \
        \             row_events AS (SELECT e.package_id                                                                       AS id, \
        \                                   e.tenant_uuid, \
        \                                   ROW_NUMBER() \
        \                                   OVER (PARTITION BY e.package_id, e.tenant_uuid ORDER BY e.created_at)              AS rn, \
        \                                   e.uuid::text                                                                       AS uuid \
        \                            FROM knowledge_model_package_event e) \
        \        SELECT COUNT(*) \
        \        INTO mismatches \
        \        FROM json_events j \
        \             JOIN row_events r \
        \                  ON j.id = r.id \
        \                      AND j.tenant_uuid = r.tenant_uuid \
        \                      AND j.rn = r.rn \
        \        WHERE j.uuid IS DISTINCT FROM r.uuid; \
        \ \
        \        IF mismatches > 0 THEN \
        \            RAISE EXCEPTION 'Test FAILED: % mismatches between JSON order and row order.', mismatches; \
        \        ELSE \
        \            RAISE NOTICE 'Test PASSED: all UUIDs have matching event order between JSON and row-based table.'; \
        \        END IF; \
        \    END; \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 4.1) DROP EVENTS COLUMN FROM PACKAGE TABLE
-- ------------------------------------------------------------------------------------------------------------------------
dropEventsColumnFromKnowledgeModelPackage dbPool = do
  let sql = "ALTER TABLE knowledge_model_package DROP COLUMN IF EXISTS events;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

dropGetBranchStateFunction dbPool = do
  let sql =
        "DROP FUNCTION IF EXISTS get_branch_state(knowledge_model_migration knowledge_model_migration, \
        \                                         branch_data branch_data, \
        \                                         fork_of_package_id varchar, \
        \                                         tenant_uuid uuid);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 4.2) DROP TABLE BRANCH DATA
-- ------------------------------------------------------------------------------------------------------------------------
dropBranchDataTable dbPool = do
  let sql = "DROP TABLE IF EXISTS branch_data;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 4.3) DROP TABLE KM_EDITOR_EVENT_UUID_MAPPING
-- ------------------------------------------------------------------------------------------------------------------------
dropKmEditorEventUuidMappingTable dbPool = do
  let sql = "DROP TABLE IF EXISTS km_editor_event_uuid_mapping;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 4.4) DROP TABLE PACKAGE_EVENT_UUID_MAPPING
-- ------------------------------------------------------------------------------------------------------------------------
dropPackageEventUuidMappingTable dbPool = do
  let sql = "DROP TABLE IF EXISTS package_event_uuid_mapping;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 5.1) RENAME branches TO knowledge_model_editors IN tenant_limit_bundle TABLE
-- ------------------------------------------------------------------------------------------------------------------------
renameToKnowledgeModelEditorsInTenantLimitBundle dbPool = do
  let sql = "ALTER TABLE tenant_limit_bundle RENAME branches to knowledge_model_editors;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 5.2) CREATE FUNCTION AND TRIGGER TO CREATE PERSISTENT COMMAND ON DOCUMENT DELETE
-- ------------------------------------------------------------------------------------------------------------------------
createPersistentCommandFromDocumentDeleteFunction dbPool = do
  let sql =
        "CREATE OR REPLACE FUNCTION create_persistent_command_from_document_delete() \
        \    RETURNS TRIGGER AS \
        \$$ \
        \BEGIN \
        \    PERFORM create_persistent_command( \
        \            'document', \
        \            'deleteFromS3', \
        \            jsonb_build_object('uuid', OLD.uuid), \
        \            OLD.tenant_uuid); \
        \    RETURN OLD; \
        \END; \
        \$$ LANGUAGE plpgsql;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createTriggerOnAfterDocumentDelete dbPool = do
  let sql =
        "CREATE OR REPLACE TRIGGER trigger_on_after_document_delete \
        \    AFTER DELETE \
        \    ON document \
        \    FOR EACH ROW \
        \EXECUTE FUNCTION create_persistent_command_from_document_delete();"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 5.2) CREATE FOREIGN KEY CONSTRAINT FROM submission TO config_submission_service WITH ON DELETE CASCADE
-- ------------------------------------------------------------------------------------------------------------------------
deleteOrphanedSubmissions dbPool = do
  let sql =
        "DELETE FROM submission \
        \WHERE NOT EXISTS ( \
        \    SELECT 1 \
        \    FROM config_submission_service \
        \    WHERE config_submission_service.tenant_uuid = submission.tenant_uuid \
        \      AND config_submission_service.id = submission.service_id \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createForeignKeyConstraintFromSubmissionToConfigSubmissionService dbPool = do
  let sql =
        "ALTER TABLE submission \
        \    ADD CONSTRAINT submission_service_id_fk FOREIGN KEY (tenant_uuid, service_id) REFERENCES config_submission_service (tenant_uuid, id) ON DELETE CASCADE"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 5.3) RENAME registry_package TO registry_knowledge_model_package
-- ------------------------------------------------------------------------------------------------------------------------
renameToRegistryKnowledgeModelPackage dbPool = do
  let sql =
        "ALTER TABLE registry_package RENAME TO registry_knowledge_model_package; \
        \ALTER TABLE registry_knowledge_model_package RENAME CONSTRAINT registry_package_pk TO registry_knowledge_model_package_pk; \
        \ALTER INDEX registry_package_id_uindex RENAME TO registry_knowledge_model_package_id_uindex;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 5.4) RENAME branch IN knowledge_model_migration TABLE TO editor
-- ------------------------------------------------------------------------------------------------------------------------
renameToBranchInKnowledgeModelMigration dbPool = do
  let sql =
        "ALTER TABLE knowledge_model_migration RENAME COLUMN branch_uuid TO editor_uuid; \
        \ALTER TABLE knowledge_model_migration RENAME COLUMN migration_state TO state; \
        \ALTER TABLE knowledge_model_migration RENAME COLUMN branch_previous_package_id TO editor_previous_package_id; \
        \ALTER TABLE knowledge_model_migration RENAME COLUMN branch_events TO editor_previous_package_events; \
        \ \
        \ALTER TABLE knowledge_model_migration RENAME CONSTRAINT knowledge_model_migration_branch_uuid_fk TO knowledge_model_migration_editor_uuid_fk; \
        \ALTER TABLE knowledge_model_migration RENAME CONSTRAINT knowledge_model_migration_branch_previous_package_id_fk TO knowledge_model_migration_editor_previous_package_id_fk; \
        \ \
        \ALTER INDEX knowledge_model_migration_branch_uuid_uindex RENAME TO knowledge_model_migration_editor_uuid_uindex;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 5.5) RENAME package CONSTRAINTS AND INDEXES TO knowledge_model_package
-- ------------------------------------------------------------------------------------------------------------------------
renamePackageConstraintsAndIndexToKnowledgeModelPackage dbPool = do
  let sql =
        "ALTER TABLE knowledge_model_package RENAME CONSTRAINT package_pk TO knowledge_model_package_pk; \
        \ALTER TABLE knowledge_model_package RENAME CONSTRAINT package_previous_package_id_fk TO knowledge_model_package_previous_package_id_fk; \
        \ALTER TABLE knowledge_model_package RENAME CONSTRAINT package_tenant_uuid_fk TO knowledge_model_package_tenant_uuid_fk; \
        \ \
        \ALTER INDEX package_id_uindex RENAME TO knowledge_model_package_id_uindex; \
        \ALTER INDEX package_organization_id_km_id_index RENAME TO knowledge_model_package_organization_id_km_id_index; \
        \ALTER INDEX package_previous_package_id_index RENAME TO knowledge_model_package_previous_package_id_index;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 5.6) RENAME branch CONSTRAINTS AND INDEXES TO knowledge_model_editor
-- ------------------------------------------------------------------------------------------------------------------------
renameBranchConstraintsAndIndexToKnowledgeModelEditor dbPool = do
  let sql =
        "ALTER TABLE knowledge_model_editor RENAME CONSTRAINT branch_pk TO knowledge_model_editor_pk; \
        \ALTER TABLE knowledge_model_editor RENAME CONSTRAINT branch_created_by_fk TO knowledge_model_editor_created_by_fk; \
        \ALTER TABLE knowledge_model_editor RENAME CONSTRAINT branch_previous_package_id_fk TO knowledge_model_editor_previous_package_id_fk; \
        \ALTER TABLE knowledge_model_editor RENAME CONSTRAINT branch_tenant_uuid_fk TO knowledge_model_editor_tenant_uuid_fk; \
        \ \
        \ALTER INDEX branch_uuid_uindex RENAME TO knowledge_model_editor_uuid_uindex;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 5.7) RENAME FOREIGN KEYS IN document_template_draft_data TABLE
-- ------------------------------------------------------------------------------------------------------------------------
renameDocumentTemplateForeignKeys dbPool = do
  let sql =
        "ALTER TABLE document_template_draft_data RENAME COLUMN branch_uuid TO knowledge_model_editor_uuid; \
        \ALTER TABLE document_template_draft_data ADD CONSTRAINT document_template_draft_data_document_template_id_fk FOREIGN KEY (document_template_id, tenant_uuid) REFERENCES document_template (id, tenant_uuid); \
        \ALTER TABLE document_template_draft_data ADD CONSTRAINT document_template_draft_data_knowledge_model_editor_uuid_fk FOREIGN KEY (knowledge_model_editor_uuid, tenant_uuid) REFERENCES knowledge_model_editor (uuid, tenant_uuid);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 5.8) RENAME event_type TO questionnaire_event_type
-- ------------------------------------------------------------------------------------------------------------------------
renameEventTypeToQuestionnaireEventType dbPool = do
  let sql = "ALTER TYPE event_type RENAME TO questionnaire_event_type;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 5.9) RENAME get_knowledge_model_editor_state FUNCTION
-- ------------------------------------------------------------------------------------------------------------------------
renameGetKnowledgeModelEditorStateFunction dbPool = do
  let sql =
        "CREATE or REPLACE FUNCTION get_knowledge_model_editor_state(knowledge_model_editor_uuid uuid, \
        \                                                            knowledge_model_migration knowledge_model_migration, \
        \                                                            fork_of_package_id varchar, \
        \                                                            editor_tenant_uuid uuid) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    state varchar; \
        \BEGIN \
        \    SELECT CASE \
        \               WHEN knowledge_model_migration.state ->> 'type' IS NOT NULL AND \
        \                    knowledge_model_migration.state ->> 'type' != 'CompletedKnowledgeModelMigrationState' THEN 'MigratingKnowledgeModelEditorState' \
        \               WHEN knowledge_model_migration.state ->> 'type' IS NOT NULL AND \
        \                    knowledge_model_migration.state ->> 'type' = 'CompletedKnowledgeModelMigrationState' THEN 'MigratedKnowledgeModelEditorState' \
        \               WHEN (SELECT COUNT(*) FROM knowledge_model_editor_event e WHERE e.tenant_uuid = editor_tenant_uuid AND e.editor_uuid = knowledge_model_editor_uuid) > 0 THEN 'EditedKnowledgeModelEditorState' \
        \               WHEN fork_of_package_id != get_newest_knowledge_model_package_2(fork_of_package_id, editor_tenant_uuid, ARRAY['ReleasedKnowledgeModelPackagePhase', 'DeprecatedKnowledgeModelPackagePhase']) THEN 'OutdatedKnowledgeModelEditorState' \
        \               WHEN True THEN 'DefaultKnowledgeModelEditorState' END \
        \    INTO state; \
        \    RETURN state; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 5.10) RENAME get_knowledge_model_editor_fork_of_package_id FUNCTION
-- ------------------------------------------------------------------------------------------------------------------------
renameGetKnowledgeModelEditorForkOfPackageIdFunction dbPool = do
  let sql =
        "DROP FUNCTION get_branch_fork_of_package_id; \
        \CREATE or REPLACE FUNCTION get_knowledge_model_editor_fork_of_package_id(config_organization config_organization, \
        \                                                                                         previous_pkg knowledge_model_package, \
        \                                                                                         knowledge_model_editor knowledge_model_editor) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    fork_of_package_id varchar; \
        \BEGIN \
        \    SELECT CASE \
        \               WHEN knowledge_model_editor.previous_package_id IS NULL THEN NULL \
        \               WHEN previous_pkg.organization_id = config_organization.organization_id AND \
        \                    previous_pkg.km_id = knowledge_model_editor.km_id THEN previous_pkg.fork_of_package_id \
        \               WHEN True THEN knowledge_model_editor.previous_package_id END as fork_of_package_id \
        \    INTO fork_of_package_id; \
        \    RETURN fork_of_package_id; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 5.11) RENAME KNOWLEDGE_MODEL_PACKAGE FUNCTIONS
-- ------------------------------------------------------------------------------------------------------------------------
renameKnowledgeModelPackageFunctions dbPool = do
  let sql =
        "DROP FUNCTION get_newest_package(req_organization_id character varying, req_km_id character varying, req_tenant_uuid uuid); \
        \DROP FUNCTION get_newest_package(req_organization_id character varying, req_km_id character varying, req_tenant_uuid uuid, req_phase character varying[]); \
        \DROP FUNCTION get_newest_package_2(req_p_id character varying, req_tenant_uuid uuid); \
        \DROP FUNCTION get_newest_package_2(req_p_id character varying, req_tenant_uuid uuid, req_phase character varying[]); \
        \ \
        \CREATE or REPLACE FUNCTION get_newest_knowledge_model_package(req_organization_id varchar, req_km_id varchar, \
        \                                                              req_tenant_uuid uuid, req_phase varchar[]) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    p_id varchar; \
        \BEGIN \
        \    SELECT CONCAT(organization_id, ':', km_id, ':', \
        \                  (max(string_to_array(version, '.')::int[]))[1] || \
        \                  '.' || \
        \                  (max(string_to_array(version, '.')::int[]))[2] || \
        \                  '.' || \
        \                  (max(string_to_array(version, '.')::int[]))[3]) \
        \    INTO p_id \
        \    FROM knowledge_model_package \
        \    WHERE organization_id = req_organization_id \
        \      AND km_id = req_km_id \
        \      AND tenant_uuid = req_tenant_uuid \
        \      AND phase = any (req_phase) \
        \    GROUP BY organization_id, km_id; \
        \    RETURN p_id; \
        \END; \
        \$$; \
        \ \
        \CREATE or REPLACE FUNCTION get_newest_knowledge_model_package_2(req_p_id varchar, req_tenant_uuid uuid, req_phase varchar[]) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    p_id varchar; \
        \BEGIN \
        \    SELECT CASE \
        \               WHEN req_p_id IS NULL THEN NULL \
        \               ELSE get_newest_knowledge_model_package(get_organization_id(req_p_id), get_km_id(req_p_id), \
        \                                                       req_tenant_uuid, req_phase) \
        \               END as newest_package_id \
        \    INTO p_id; \
        \    RETURN p_id; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 5.12) UPDATE KNOWLEDGE_MODEL_PACKAGE PHASE
-- ------------------------------------------------------------------------------------------------------------------------
updateKnowledgeModelPackagePhase dbPool = do
  let sql =
        "UPDATE knowledge_model_package SET phase = 'ReleasedKnowledgeModelPackagePhase' WHERE phase = 'ReleasedPackagePhase'; \
        \UPDATE knowledge_model_package SET phase = 'DeprecatedKnowledgeModelPackagePhase' WHERE phase = 'DeprecatedPackagePhase';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 5.14) RENAME PACKAGE_ID AT FEEDBACK TABLE
-- ------------------------------------------------------------------------------------------------------------------------
renamePackageIdAtFeedback dbPool = do
  let sql =
        "ALTER TABLE feedback RENAME package_id TO knowledge_model_package_id; \
        \ALTER TABLE feedback RENAME CONSTRAINT feedback_package_id_fk TO feedback_knowledge_model_package_id_fk; \
        \ALTER INDEX feedback_package_id_index RENAME TO feedback_knowledge_model_package_id_index;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 5.14) RENAME PACKAGE_ID AT QUESTIONNAIRE TABLE
-- ------------------------------------------------------------------------------------------------------------------------
renamePackageIdAtQuestionnaire dbPool = do
  let sql =
        "ALTER TABLE questionnaire RENAME package_id TO knowledge_model_package_id; \
        \ALTER TABLE questionnaire RENAME CONSTRAINT questionnaire_package_id_fk TO questionnaire_knowledge_model_package_id_fk;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 5.15) ADD ON DELETE CASCADE TO PACKAGE_ID IN KNOWLEDGE_MODEL_CACHE TABLE
-- ------------------------------------------------------------------------------------------------------------------------
addOnDeleteCascadeToPackageIdInKnowledgeModelCache dbPool = do
  let sql =
        "ALTER TABLE knowledge_model_cache DROP CONSTRAINT knowledge_model_cache_package_id_fk; \
        \ALTER TABLE knowledge_model_cache ADD CONSTRAINT knowledge_model_cache_package_id_fk FOREIGN KEY (package_id, tenant_uuid) REFERENCES knowledge_model_package ON DELETE CASCADE;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 5.16) UPGRADE KM metamodel
-- ------------------------------------------------------------------------------------------------------------------------
upgradeKmMetamodel dbPool = do
  let sql =
        "TRUNCATE knowledge_model_cache; \
        \TRUNCATE knowledge_model_migration;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
