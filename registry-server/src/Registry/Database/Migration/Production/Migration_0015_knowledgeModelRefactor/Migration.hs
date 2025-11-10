module Registry.Database.Migration.Production.Migration_0015_knowledgeModelRefactor.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 15, mmName = "Refactor knowledge model", mmDescription = "Refactor knowledge model to use new schema"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  -- KM PACKAGE
  renameTablePackageToKnowledgeModelPackage dbPool
  createKnowledgeModelPackageEventTable dbPool
  createKnowledgeModelPackageEventMappingTable dbPool
  fillKmPackageEventUuidMappingTable dbPool
  fixKmPackageEventUuidMappingDuplicateUuids dbPool
  fixKmPackageEventUuidMappingCreatedAtTimestamps dbPool
  moveDataToKmPackageEvents dbPool
  -- KM PACKAGE TEST
  testKmPackageEventCount dbPool
  testKmPackageEventOrder dbPool
  -- CLEANUP
  dropEventsColumnFromKnowledgeModelPackage dbPool
  dropPackageEventUuidMappingTable dbPool
  -- OTHER
  refactorAudit dbPool

-- ------------------------------------------------------------------------------------------------------------------------
-- 1.1) RENAME TABLE package TO knowledge_model_package
-- ------------------------------------------------------------------------------------------------------------------------
renameTablePackageToKnowledgeModelPackage dbPool = do
  let sql = "ALTER TABLE package RENAME TO knowledge_model_package;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 1.2) CREATE KM PACKAGE EVENT TABLE
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
        \    CONSTRAINT knowledge_model_package_event_pk PRIMARY KEY (uuid, package_id), \
        \    CONSTRAINT knowledge_model_package_event_package_id_fk FOREIGN KEY (package_id) REFERENCES knowledge_model_package (id) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 1.3) CREATE KM PACKAGE EVENT MAPPING TABLE
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
        \    CONSTRAINT package_event_uuid_mapping_pk PRIMARY KEY (package_id, old_uuid, ord) \
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
-- 1.4) MOVE DATA TO KM_PACKAGE_EVENTS
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
-- 2.1) TEST KM PACKAGE EVENT COUNT
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
-- 2.2) TEST KM PACKAGE EVENT ORDER
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
-- 3.1) DROP EVENTS COLUMN FROM PACKAGE TABLE
-- ------------------------------------------------------------------------------------------------------------------------
dropEventsColumnFromKnowledgeModelPackage dbPool = do
  let sql = "ALTER TABLE knowledge_model_package DROP COLUMN IF EXISTS events;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 3.2) DROP TABLE PACKAGE_EVENT_UUID_MAPPING
-- ------------------------------------------------------------------------------------------------------------------------
dropPackageEventUuidMappingTable dbPool = do
  let sql = "DROP TABLE IF EXISTS package_event_uuid_mapping;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------
-- 4) OTHER
-- ------------------------------------------------------------------------------------------------------------------------
refactorAudit dbPool = do
  let sql =
        "ALTER TABLE audit RENAME COLUMN package_id to knowledge_model_package_id; \
        \ALTER TABLE audit RENAME COLUMN package_count to knowledge_model_package_count; \
        \ALTER TABLE audit RENAME COLUMN branch_count to knowledge_model_editor_count;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
