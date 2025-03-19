module Wizard.Database.Migration.Production.Migration_0053_qtnEventsRefactoring.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 53, mmName = "Refactor questionnaire events", mmDescription = "Refactor questionnaire events DB model"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  polishQuestionnaireEvents dbPool
  createQuestionnaireEventTable dbPool
  createAndInsertQuestionnaireEventUuidMappingTable dbPool
  insertQuestionnaireEvents dbPool
  createQuestionnaireEventIndex dbPool
  testEventCount dbPool
  testValueCount dbPool
  testValueOrder dbPool
  changePasswordHashForSystemUser dbPool
  createExternalLinkTable dbPool
  createVersionTable dbPool
  insertVersions dbPool
  testVersionCount dbPool
  updateDocumentQuestionnaireEventUuid dbPool
  dropQuestionnaireEventsColumn dbPool
  dropQuestionnaireVersionColumn dbPool

polishQuestionnaireEvents dbPool = do
  let sql =
        "WITH duplicate_events AS (SELECT uuid            as questionnaire_uuid, \
        \                                 elem ->> 'uuid' as event_uuid, \
        \                                 array_agg(o)    as order, \
        \                                 COUNT(o)        as len \
        \                          FROM questionnaire \
        \                               LEFT JOIN LATERAL jsonb_array_elements(events) WITH ORDINALITY AS values(elem, o) ON TRUE \
        \                          GROUP BY uuid, event_uuid \
        \                          HAVING COUNT(o) > 1 \
        \                          ORDER BY len DESC), \
        \ \
        \     indices_to_regenerate AS (SELECT duplicate_events.questionnaire_uuid                                     as questionnaire_uuid, \
        \                                   duplicate_events.event_uuid                                             as event_uuid, \
        \                                   unnest(array_remove(duplicate_events.order, duplicate_events.order[1])) as order, \
        \                                   gen_random_uuid()                                                       AS new_uuid \
        \                            FROM duplicate_events), \
        \ \
        \     updated_event AS (SELECT indices_to_regenerate.questionnaire_uuid                     AS questionnaire_uuid, \
        \                              indices_to_regenerate.event_uuid                             AS original_event_uuid, \
        \                              indices_to_regenerate.new_uuid                               AS new_uuid, \
        \                              indices_to_regenerate.order, \
        \                              questionnaire.events -> indices_to_regenerate.order::integer AS original_event, \
        \                              jsonb_set(questionnaire.events -> indices_to_regenerate.order::integer, '{uuid}', \
        \                                        to_jsonb(new_uuid), false)                      AS new_event \
        \                       FROM indices_to_regenerate \
        \                            JOIN questionnaire ON indices_to_regenerate.questionnaire_uuid = questionnaire.uuid), \
        \ \
        \     original_event AS (SELECT updated_event.questionnaire_uuid, \
        \                               elem ->> 'uuid' as event_uuid, \
        \                               elem            as event, \
        \                               o               as order \
        \                        FROM updated_event \
        \                             JOIN questionnaire ON updated_event.questionnaire_uuid = questionnaire.uuid \
        \                             LEFT JOIN LATERAL jsonb_array_elements(questionnaire.events) WITH ORDINALITY AS values(elem, o) \
        \                                       ON TRUE), \
        \ \
        \     result_event AS (SELECT original_event.*, \
        \                             updated_event.new_uuid, \
        \                             CASE \
        \                                 WHEN updated_event.new_event IS NULL THEN original_event.event \
        \                                 ELSE updated_event.new_event \
        \                                 END AS new_event \
        \                      FROM original_event \
        \                           LEFT JOIN updated_event \
        \                                     ON original_event.questionnaire_uuid = updated_event.questionnaire_uuid AND \
        \                                        original_event.order = updated_event.order \
        \                      ORDER BY original_event.order), \
        \ \
        \     final_events AS (SELECT result_event.questionnaire_uuid, \
        \                             json_agg(result_event.new_event) as events \
        \                      FROM result_event \
        \                      GROUP BY result_event.questionnaire_uuid) \
        \ \
        \UPDATE questionnaire \
        \SET events = final_events.events \
        \FROM final_events \
        \WHERE questionnaire.uuid = final_events.questionnaire_uuid;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createQuestionnaireEventTable dbPool = do
  let sql =
        "CREATE TYPE event_type AS ENUM ('ClearReplyEvent', 'SetReplyEvent', 'SetLabelsEvent', 'SetPhaseEvent'); \
        \CREATE TYPE value_type AS ENUM ('IntegrationReply', 'AnswerReply', 'MultiChoiceReply', 'ItemListReply', 'StringReply', 'ItemSelectReply', 'FileReply'); \
        \ \
        \CREATE TABLE IF NOT EXISTS questionnaire_event \
        \( \
        \    uuid               uuid                     NOT NULL, \
        \    event_type         event_type               NOT NULL, \
        \    path               text, \
        \    created_at         timestamp with time zone NOT NULL, \
        \    created_by         uuid, \
        \    questionnaire_uuid uuid                     NOT NULL, \
        \    tenant_uuid        uuid                     NOT NULL, \
        \    value_type         value_type, \
        \    value              text[], \
        \    value_id           text, \
        \    CONSTRAINT questionnaire_event_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_event_created_by_fk FOREIGN KEY (created_by, tenant_uuid) references user_entity (uuid, tenant_uuid) ON DELETE SET NULL, \
        \    CONSTRAINT questionnaire_event_questionnaire_uuid_fk FOREIGN KEY (questionnaire_uuid, tenant_uuid) references questionnaire (uuid, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT questionnaire_event_tenant_uuid_fk FOREIGN KEY (tenant_uuid) references tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createAndInsertQuestionnaireEventUuidMappingTable dbPool = do
  let sql =
        "CREATE TEMP TABLE questionnaire_event_uuid_mapping \
        \( \
        \    tenant_uuid        UUID NOT NULL, \
        \    questionnaire_uuid UUID NOT NULL, \
        \    old_uuid           UUID NOT NULL, \
        \    new_uuid           UUID NOT NULL, \
        \    CONSTRAINT questionnaire_event_uuid_mapping_pk PRIMARY KEY (tenant_uuid, questionnaire_uuid, old_uuid) \
        \); \
        \ \
        \WITH extracted_events AS (SELECT q.tenant_uuid            AS tenant_uuid, \
        \                                 q.uuid                   AS questionnaire_uuid, \
        \                                 (event ->> 'uuid')::UUID AS old_uuid, \
        \                                 gen_random_uuid()        AS new_uuid \
        \                          FROM questionnaire q, \
        \                               LATERAL jsonb_array_elements(q.events) event) \
        \INSERT \
        \INTO questionnaire_event_uuid_mapping (tenant_uuid, questionnaire_uuid, old_uuid, new_uuid) \
        \SELECT extracted_events.tenant_uuid, extracted_events.questionnaire_uuid, extracted_events.old_uuid, extracted_events.new_uuid \
        \FROM extracted_events \
        \ON CONFLICT DO NOTHING;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

insertQuestionnaireEvents dbPool = do
  let sql =
        "WITH extracted_event AS (SELECT (elem ->> 'uuid')::uuid                          AS uuid, \
        \                                (elem ->> 'type')::event_type                    AS event_type, \
        \                                elem ->> 'path'                                  AS path, \
        \                                (elem ->> 'createdAt')::TIMESTAMP WITH TIME ZONE AS created_at, \
        \                                (elem ->> 'createdBy')::uuid                     AS created_by, \
        \                                questionnaire.uuid                               AS questionnaire_uuid, \
        \                                questionnaire.tenant_uuid                        AS tenant_uuid, \
        \                                (elem -> 'value' ->> 'type')::value_type         AS value_type, \
        \                                CASE \
        \                                    WHEN elem -> 'value' ->> 'type' = 'IntegrationReply' THEN \
        \                                        CASE \
        \                                            WHEN elem -> 'value' -> 'value' ->> 'value' = '' THEN '{\"\"}'::text[] \
        \                                            WHEN elem -> 'value' -> 'value' ->> 'value' IS NULL THEN NULL \
        \                                            ELSE array_agg(elem -> 'value' -> 'value' ->> 'value') \
        \                                            END \
        \                                    WHEN elem -> 'value' ->> 'type' IN \
        \                                         ('AnswerReply', 'StringReply', 'ItemSelectReply', 'FileReply') THEN \
        \                                        CASE \
        \                                            WHEN elem -> 'value' ->> 'value' = '' THEN '{\"\"}'::text[] \
        \                                            WHEN elem -> 'value' ->> 'value' IS NULL THEN NULL \
        \                                            ELSE array_agg(elem -> 'value' ->> 'value') \
        \                                            END \
        \                                    END                                          AS value, \
        \                                elem -> 'value' -> 'value' ->> 'id'              AS value_id \
        \                         FROM questionnaire \
        \                              LEFT JOIN LATERAL jsonb_array_elements(events) AS elem ON TRUE \
        \                         WHERE elem -> 'value' IS NOT NULL \
        \                           AND elem -> 'value' ->> 'type' IN \
        \                               ('IntegrationReply', 'AnswerReply', 'StringReply', 'ItemSelectReply', 'FileReply') \
        \                         GROUP BY questionnaire.uuid, questionnaire.tenant_uuid, elem) \
        \INSERT \
        \INTO questionnaire_event (uuid, event_type, path, created_at, created_by, questionnaire_uuid, tenant_uuid, \
        \                          value_type, value, value_id) \
        \SELECT questionnaire_event_uuid_mapping.new_uuid, \
        \       extracted_event.event_type, \
        \       extracted_event.path, \
        \       extracted_event.created_at, \
        \       user_entity.uuid, \
        \       extracted_event.questionnaire_uuid, \
        \       extracted_event.tenant_uuid, \
        \       extracted_event.value_type, \
        \       extracted_event.value, \
        \       extracted_event.value_id \
        \FROM extracted_event \
        \     LEFT JOIN questionnaire_event_uuid_mapping \
        \               ON questionnaire_event_uuid_mapping.tenant_uuid = extracted_event.tenant_uuid AND \
        \                  questionnaire_event_uuid_mapping.questionnaire_uuid = extracted_event.questionnaire_uuid AND \
        \                  questionnaire_event_uuid_mapping.old_uuid = extracted_event.uuid \
        \     LEFT JOIN user_entity ON extracted_event.created_by = user_entity.uuid AND \
        \                              extracted_event.tenant_uuid = user_entity.tenant_uuid \
        \ON CONFLICT DO NOTHING; \
        \ \
        \WITH extracted_event AS (SELECT (elem ->> 'uuid')::uuid                          AS uuid, \
        \                                (elem ->> 'type')::event_type                    AS event_type, \
        \                                elem ->> 'path'                                  AS path, \
        \                                (elem ->> 'createdAt')::TIMESTAMP WITH TIME ZONE AS created_at, \
        \                                (elem ->> 'createdBy')::uuid                     AS created_by, \
        \                                questionnaire.uuid                               AS questionnaire_uuid, \
        \                                questionnaire.tenant_uuid                        AS tenant_uuid, \
        \                                (elem -> 'value' ->> 'type')::value_type         AS value_type, \
        \                                CASE \
        \                                    WHEN jsonb_array_length(elem -> 'value' -> 'value') = 0 THEN '{}'::text[] \
        \                                    ELSE array_agg(v ORDER BY o) \
        \                                    END                                          AS value \
        \ \
        \                         FROM questionnaire \
        \                              LEFT JOIN LATERAL jsonb_array_elements(events) AS elem ON TRUE \
        \                              LEFT JOIN LATERAL jsonb_array_elements_text(elem -> 'value' -> 'value') WITH ORDINALITY AS values(v, o) \
        \                                        ON TRUE \
        \                         WHERE elem -> 'value' IS NOT NULL \
        \                           AND elem -> 'value' ->> 'type' = 'MultiChoiceReply' \
        \                         GROUP BY questionnaire.uuid, questionnaire.tenant_uuid, elem) \
        \INSERT \
        \INTO questionnaire_event (uuid, event_type, path, created_at, created_by, questionnaire_uuid, tenant_uuid, \
        \                          value_type, value) \
        \SELECT questionnaire_event_uuid_mapping.new_uuid, \
        \       extracted_event.event_type, \
        \       extracted_event.path, \
        \       extracted_event.created_at, \
        \       user_entity.uuid, \
        \       extracted_event.questionnaire_uuid, \
        \       extracted_event.tenant_uuid, \
        \       extracted_event.value_type, \
        \       extracted_event.value \
        \FROM extracted_event \
        \     LEFT JOIN questionnaire_event_uuid_mapping \
        \               ON questionnaire_event_uuid_mapping.tenant_uuid = extracted_event.tenant_uuid AND \
        \                  questionnaire_event_uuid_mapping.questionnaire_uuid = extracted_event.questionnaire_uuid AND \
        \                  questionnaire_event_uuid_mapping.old_uuid = extracted_event.uuid \
        \     LEFT JOIN user_entity ON extracted_event.created_by = user_entity.uuid AND \
        \                              extracted_event.tenant_uuid = user_entity.tenant_uuid \
        \ON CONFLICT DO NOTHING; \
        \ \
        \WITH extracted_event AS (SELECT (elem ->> 'uuid')::uuid                          AS uuid, \
        \                                (elem ->> 'type')::event_type                    AS event_type, \
        \                                elem ->> 'path'                                  AS path, \
        \                                (elem ->> 'createdAt')::TIMESTAMP WITH TIME ZONE AS created_at, \
        \                                (elem ->> 'createdBy')::uuid                     AS created_by, \
        \                                questionnaire.uuid                               AS questionnaire_uuid, \
        \                                questionnaire.tenant_uuid                        AS tenant_uuid, \
        \                                (elem -> 'value' ->> 'type')::value_type         AS value_type, \
        \                                CASE \
        \                                    WHEN jsonb_array_length(elem -> 'value' -> 'value') = 0 THEN '{}'::text[] \
        \                                    ELSE array_agg(v ORDER BY o) \
        \                                    END                                          AS value \
        \                         FROM questionnaire \
        \                              LEFT JOIN LATERAL jsonb_array_elements(events) AS elem ON TRUE \
        \                              LEFT JOIN LATERAL jsonb_array_elements_text(elem -> 'value' -> 'value') WITH ORDINALITY AS values(v, o) \
        \                                        ON TRUE \
        \                         WHERE elem -> 'value' IS NOT NULL \
        \                           AND elem -> 'value' ->> 'type' = 'ItemListReply' \
        \                         GROUP BY questionnaire.uuid, questionnaire.tenant_uuid, elem) \
        \INSERT \
        \INTO questionnaire_event (uuid, event_type, path, created_at, created_by, questionnaire_uuid, tenant_uuid, \
        \                          value_type, value) \
        \SELECT questionnaire_event_uuid_mapping.new_uuid, \
        \       extracted_event.event_type, \
        \       extracted_event.path, \
        \       extracted_event.created_at, \
        \       user_entity.uuid, \
        \       extracted_event.questionnaire_uuid, \
        \       extracted_event.tenant_uuid, \
        \       extracted_event.value_type, \
        \       extracted_event.value \
        \FROM extracted_event \
        \     LEFT JOIN questionnaire_event_uuid_mapping \
        \               ON questionnaire_event_uuid_mapping.tenant_uuid = extracted_event.tenant_uuid AND \
        \                  questionnaire_event_uuid_mapping.questionnaire_uuid = extracted_event.questionnaire_uuid AND \
        \                  questionnaire_event_uuid_mapping.old_uuid = extracted_event.uuid \
        \     LEFT JOIN user_entity ON extracted_event.created_by = user_entity.uuid AND \
        \                              extracted_event.tenant_uuid = user_entity.tenant_uuid \
        \ON CONFLICT DO NOTHING; \
        \ \
        \WITH extracted_event AS (SELECT (elem ->> 'uuid')::uuid                          AS uuid, \
        \                                (elem ->> 'type')::event_type                    AS event_type, \
        \                                elem ->> 'path'                                  AS path, \
        \                                (elem ->> 'createdAt')::TIMESTAMP WITH TIME ZONE AS created_at, \
        \                                (elem ->> 'createdBy')::uuid                     AS created_by, \
        \                                questionnaire.uuid                               AS questionnaire_uuid, \
        \                                questionnaire.tenant_uuid                        AS tenant_uuid, \
        \                                CASE \
        \                                    WHEN jsonb_array_length(elem -> 'value') = 0 THEN '{}'::text[] \
        \                                    ELSE array_agg(v ORDER BY o) \
        \                                    END                                          AS value \
        \                         FROM questionnaire \
        \                              LEFT JOIN LATERAL jsonb_array_elements(events) AS elem ON TRUE \
        \                              LEFT JOIN LATERAL jsonb_array_elements_text(elem -> 'value') WITH ORDINALITY AS values(v, o) \
        \                                        ON TRUE \
        \                         WHERE elem -> 'value' IS NOT NULL \
        \                           AND elem ->> 'type' = 'SetLabelsEvent' \
        \                         GROUP BY questionnaire.uuid, questionnaire.tenant_uuid, elem) \
        \INSERT \
        \INTO questionnaire_event (uuid, event_type, path, created_at, created_by, questionnaire_uuid, tenant_uuid, value) \
        \SELECT questionnaire_event_uuid_mapping.new_uuid, \
        \       extracted_event.event_type, \
        \       extracted_event.path, \
        \       extracted_event.created_at, \
        \       user_entity.uuid, \
        \       extracted_event.questionnaire_uuid, \
        \       extracted_event.tenant_uuid, \
        \       extracted_event.value \
        \FROM extracted_event \
        \     LEFT JOIN questionnaire_event_uuid_mapping \
        \               ON questionnaire_event_uuid_mapping.tenant_uuid = extracted_event.tenant_uuid AND \
        \                  questionnaire_event_uuid_mapping.questionnaire_uuid = extracted_event.questionnaire_uuid AND \
        \                  questionnaire_event_uuid_mapping.old_uuid = extracted_event.uuid \
        \     LEFT JOIN user_entity ON extracted_event.created_by = user_entity.uuid AND \
        \                              extracted_event.tenant_uuid = user_entity.tenant_uuid \
        \ON CONFLICT DO NOTHING; \
        \ \
        \WITH extracted_event AS (SELECT (elem ->> 'uuid')::uuid                          AS uuid, \
        \                                (elem ->> 'type')::event_type                    AS event_type, \
        \                                elem ->> 'path'                                  AS path, \
        \                                (elem ->> 'createdAt')::TIMESTAMP WITH TIME ZONE AS created_at, \
        \                                (elem ->> 'createdBy')::uuid                     AS created_by, \
        \                                questionnaire.uuid                               AS questionnaire_uuid, \
        \                                questionnaire.tenant_uuid                        AS tenant_uuid, \
        \                                CASE \
        \                                    WHEN elem ->> 'phaseUuid' IS NULL THEN NULL \
        \                                    ELSE ARRAY[elem->>'phaseUuid'] \
        \                                    END                                          AS value \
        \                         FROM questionnaire \
        \                              LEFT JOIN LATERAL jsonb_array_elements(events) AS elem ON TRUE \
        \                         WHERE elem ->> 'type' = 'SetPhaseEvent' \
        \                         GROUP BY questionnaire.uuid, questionnaire.tenant_uuid, elem) \
        \INSERT \
        \INTO questionnaire_event (uuid, event_type, path, created_at, created_by, questionnaire_uuid, tenant_uuid, value) \
        \SELECT questionnaire_event_uuid_mapping.new_uuid, \
        \       extracted_event.event_type, \
        \       extracted_event.path, \
        \       extracted_event.created_at, \
        \       user_entity.uuid, \
        \       extracted_event.questionnaire_uuid, \
        \       extracted_event.tenant_uuid, \
        \       extracted_event.value \
        \FROM extracted_event \
        \     LEFT JOIN questionnaire_event_uuid_mapping \
        \               ON questionnaire_event_uuid_mapping.tenant_uuid = extracted_event.tenant_uuid AND \
        \                  questionnaire_event_uuid_mapping.questionnaire_uuid = extracted_event.questionnaire_uuid AND \
        \                  questionnaire_event_uuid_mapping.old_uuid = extracted_event.uuid \
        \     LEFT JOIN user_entity ON extracted_event.created_by = user_entity.uuid AND \
        \                              extracted_event.tenant_uuid = user_entity.tenant_uuid \
        \ON CONFLICT DO NOTHING; \
        \ \
        \WITH questionnaire_event AS (SELECT (elem ->> 'uuid')::uuid                          AS uuid, \
        \                                    (elem ->> 'type')::event_type                    AS event_type, \
        \                                    elem ->> 'path'                                  AS path, \
        \                                    (elem ->> 'createdAt')::TIMESTAMP WITH TIME ZONE AS created_at, \
        \                                    (elem ->> 'createdBy')::uuid                     AS created_by, \
        \                                    questionnaire.uuid                               AS questionnaire_uuid, \
        \                                    questionnaire.tenant_uuid                        AS tenant_uuid \
        \                             FROM questionnaire \
        \                                  LEFT JOIN LATERAL jsonb_array_elements(events) AS elem ON TRUE \
        \                             WHERE elem ->> 'type' = 'ClearReplyEvent' \
        \                             GROUP BY questionnaire.uuid, questionnaire.tenant_uuid, elem) \
        \INSERT \
        \INTO questionnaire_event (uuid, event_type, path, created_at, created_by, questionnaire_uuid, tenant_uuid) \
        \SELECT questionnaire_event_uuid_mapping.new_uuid, \
        \       questionnaire_event.event_type, \
        \       questionnaire_event.path, \
        \       questionnaire_event.created_at, \
        \       user_entity.uuid, \
        \       questionnaire_event.questionnaire_uuid, \
        \       questionnaire_event.tenant_uuid \
        \FROM questionnaire_event \
        \     LEFT JOIN questionnaire_event_uuid_mapping \
        \               ON questionnaire_event_uuid_mapping.tenant_uuid = questionnaire_event.tenant_uuid AND \
        \                  questionnaire_event_uuid_mapping.questionnaire_uuid = questionnaire_event.questionnaire_uuid AND \
        \                  questionnaire_event_uuid_mapping.old_uuid = questionnaire_event.uuid \
        \     LEFT JOIN user_entity ON questionnaire_event.created_by = user_entity.uuid AND \
        \                              questionnaire_event.tenant_uuid = user_entity.tenant_uuid \
        \ON CONFLICT DO NOTHING;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createQuestionnaireEventIndex dbPool = do
  let sql = "CREATE INDEX questionnaire_event_questionnaire_uuid_tenant_uuid_index ON questionnaire_event USING BTREE (questionnaire_uuid, tenant_uuid);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

testEventCount dbPool = do
  let sql =
        "DO \
        \$$ \
        \    BEGIN \
        \        IF NOT EXISTS(SELECT t1.uuid, \
        \             t1.event_count AS event_count_original, \
        \             t2.event_count AS event_count \
        \        FROM (SELECT uuid, COALESCE(jsonb_array_length((SELECT jsonb_agg(DISTINCT elem->'uuid') FROM jsonb_array_elements(events) elem)), 0) AS event_count \
        \            FROM questionnaire) AS t1 \
        \           FULL OUTER JOIN \
        \           (SELECT questionnaire_uuid, COALESCE(COUNT(*), 0) AS event_count \
        \            FROM questionnaire_event \
        \            GROUP BY questionnaire_uuid) AS t2 \
        \           ON t1.uuid = t2.questionnaire_uuid \
        \        WHERE (t1.event_count <> t2.event_count) \
        \         OR (t1.event_count IS NULL) \
        \         OR (t2.event_count IS NULL) AND (t1.event_count <> 0)) THEN \
        \            RAISE NOTICE 'Test PASSED: Query returned no mismatched event counts.'; \
        \        ELSE \
        \            RAISE EXCEPTION 'Test FAILED: Query returned mismatched event counts.'; \
        \        END IF; \
        \    END; \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

testValueCount dbPool = do
  let sql =
        "DO \
        \$$ \
        \    DECLARE \
        \        mismatch_count INT; \
        \    BEGIN \
        \        SELECT COUNT(*) \
        \        INTO mismatch_count \
        \        FROM (WITH t1_with_old_uuids AS (SELECT questionnaire.uuid           AS questionnaire_uuid, \
        \                                                questionnaire.tenant_uuid    AS tenant_uuid, \
        \                                                (elem ->> 'uuid')::uuid      AS uuid, \
        \                                                (elem ->> 'phaseUuid')::uuid AS phase_uuid, \
        \                                                SUM( \
        \                                                        CASE \
        \                                                            WHEN jsonb_typeof(elem -> 'value') = 'array' \
        \                                                                THEN jsonb_array_length(elem -> 'value') \
        \                                                            WHEN jsonb_typeof(elem -> 'value') = 'string' THEN 1 \
        \                                                            WHEN jsonb_typeof(elem -> 'value') = 'object' AND \
        \                                                                 jsonb_typeof(elem -> 'value' -> 'value') = \
        \                                                                 'object' THEN 1 \
        \                                                            WHEN jsonb_typeof(elem -> 'value') = 'object' AND \
        \                                                                 jsonb_typeof(elem -> 'value' -> 'value') = \
        \                                                                 'string' THEN 1 \
        \                                                            WHEN jsonb_typeof(elem -> 'value') = 'object' AND \
        \                                                                 jsonb_typeof(elem -> 'value' -> 'value') = \
        \                                                                 'array' \
        \                                                                THEN jsonb_array_length(elem -> 'value' -> 'value') \
        \                                                            ELSE 0 \
        \                                                            END \
        \                                                )                            AS value_count \
        \                                         FROM questionnaire, \
        \                                              jsonb_array_elements(events) AS elem \
        \                                         GROUP BY tenant_uuid, questionnaire_uuid, elem ->> 'uuid', \
        \                                                  elem ->> 'phaseUuid'), \
        \                   t1 AS (SELECT questionnaire_event_uuid_mapping.new_uuid as uuid, \
        \                                 t1_with_old_uuids.phase_uuid, \
        \                                 t1_with_old_uuids.value_count \
        \                          FROM t1_with_old_uuids \
        \                               LEFT JOIN questionnaire_event_uuid_mapping \
        \                                         ON questionnaire_event_uuid_mapping.tenant_uuid = \
        \                                            t1_with_old_uuids.tenant_uuid AND \
        \                                            questionnaire_event_uuid_mapping.questionnaire_uuid = \
        \                                            t1_with_old_uuids.questionnaire_uuid AND \
        \                                            questionnaire_event_uuid_mapping.old_uuid = t1_with_old_uuids.uuid), \
        \                   t2 AS (SELECT uuid, event_type, array_length(value, 1) AS value_count \
        \                          FROM questionnaire_event \
        \                          GROUP BY uuid, event_type, value) \
        \              SELECT t1.uuid, \
        \                     t1.phase_uuid, \
        \                     t1.value_count AS value_count_original, \
        \                     t2.value_count AS value_count, \
        \                     t2.event_type  AS event_type \
        \              FROM t1 \
        \                   FULL OUTER JOIN t2 ON t1.uuid = t2.uuid \
        \              WHERE (t2.event_type = 'ClearReplyEvent' AND t1.value_count <> t2.value_count) \
        \                 OR (t2.event_type = 'SetPhaseEvent' AND t1.phase_uuid IS NULL AND t2.value_count <> 0) \
        \                 OR (t2.event_type = 'SetPhaseEvent' AND t1.phase_uuid IS NOT NULL AND t2.value_count <> 1) \
        \                 OR (t2.event_type = 'SetLabelsEvent' AND t1.value_count <> t2.value_count) \
        \                 OR (t2.event_type = 'SetReplyEvent' AND t1.value_count <> t2.value_count)) AS mismatches; \
        \ \
        \        IF mismatch_count = 0 THEN \
        \            RAISE NOTICE 'Test PASSED: Query returned no mismatched value counts.'; \
        \        ELSE \
        \            RAISE EXCEPTION 'Test FAILED: Query returned mismatched value counts.'; \
        \        END IF; \
        \    END \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

testValueOrder dbPool = do
  let sql =
        "DO \
        \$$ \
        \    DECLARE \
        \        mismatch_count INT; \
        \    BEGIN \
        \        SELECT COUNT(*) \
        \        INTO mismatch_count \
        \        FROM (WITH json1_elements AS ( \
        \            SELECT \
        \                v->>'uuid' as uuid, \
        \                o, \
        \                val as value \
        \            FROM questionnaire t1 \
        \            JOIN LATERAL jsonb_array_elements(t1.events) as v ON TRUE \
        \            JOIN LATERAL jsonb_array_elements(v->'value'->'value') WITH ORDINALITY AS values(val, o) ON TRUE \
        \            WHERE v->'value'->>'type' = 'ItemListReply' \
        \        ), \
        \        json2_elements AS ( \
        \            SELECT \
        \                uuid::text as uuid, \
        \                o, \
        \                v \
        \            FROM questionnaire_event t2 \
        \            JOIN LATERAL jsonb_array_elements(to_jsonb(t2.value)) WITH ORDINALITY AS values(v, o) ON TRUE \
        \            WHERE t2.value_type = 'ItemListReply' \
        \        ) \
        \        SELECT \
        \            json1_elements.value, \
        \            json2_elements.v, \
        \            json1_elements.o, \
        \            json2_elements.o \
        \        FROM json1_elements, json2_elements \
        \        WHERE \
        \        (json1_elements.uuid = json2_elements.uuid \
        \        AND json1_elements.value = json2_elements.v \
        \        AND json1_elements.o <> json2_elements.o)) AS mismatches; \
        \ \
        \IF mismatch_count = 0 THEN \
        \            RAISE NOTICE 'Test PASSED: Query returned no mismatched value order counts.'; \
        \        ELSE \
        \            RAISE EXCEPTION 'Test FAILED: Query returned mismatched value order counts.'; \
        \        END IF; \
        \    END \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

changePasswordHashForSystemUser dbPool = do
  let sql = "UPDATE user_entity SET password_hash = 'no-hash' WHERE uuid = '00000000-0000-0000-0000-000000000000' AND tenant_uuid = '00000000-0000-0000-0000-000000000000';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createExternalLinkTable dbPool = do
  let sql =
        "CREATE TABLE external_link_usage \
        \( \
        \    uuid        uuid        NOT NULL, \
        \    url         varchar     NOT NULL, \
        \    tenant_uuid uuid        NOT NULL, \
        \    created_at  timestamptz NOT NULL, \
        \    CONSTRAINT external_link_usage_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT external_link_usage_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createVersionTable dbPool = do
  let sql =
        "CREATE TABLE questionnaire_version \
        \( \
        \    uuid               uuid        NOT NULL, \
        \    name               varchar     NOT NULL, \
        \    description        varchar, \
        \    event_uuid         uuid        NOT NULL, \
        \    questionnaire_uuid uuid        NOT NULL, \
        \    tenant_uuid        uuid        NOT NULL, \
        \    created_by         uuid, \
        \    created_at         timestamptz NOT NULL, \
        \    updated_at         timestamptz NOT NULL, \
        \    CONSTRAINT questionnaire_version_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_version_event_uuid_fk FOREIGN KEY (event_uuid, tenant_uuid) REFERENCES questionnaire_event (uuid, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT questionnaire_version_questionnaire_uuid_fk FOREIGN KEY (questionnaire_uuid, tenant_uuid) REFERENCES questionnaire (uuid, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT questionnaire_version_created_by_fk FOREIGN KEY (created_by, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid) ON DELETE SET NULL, \
        \    CONSTRAINT questionnaire_version_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

insertVersions dbPool = do
  let sql =
        "WITH extracted_version AS (SELECT version ->> 'name'                     AS name, \
        \                                  version ->> 'description'              AS description, \
        \                                  (version ->> 'eventUuid')::UUID        AS event_uuid, \
        \                                  questionnaire.uuid                     AS questionnaire_uuid, \
        \                                  questionnaire.tenant_uuid              as tenant_uuid, \
        \                                  (version ->> 'createdBy')::UUID        AS created_by, \
        \                                  (version ->> 'createdAt')::TIMESTAMPTZ AS created_at, \
        \                                  (version ->> 'updatedAt')::TIMESTAMPTZ AS updated_at \
        \                           FROM questionnaire, \
        \                                LATERAL jsonb_array_elements(questionnaire.versions) version) \
        \INSERT \
        \INTO questionnaire_version (uuid, name, description, event_uuid, questionnaire_uuid, tenant_uuid, created_by, \
        \                            created_at, updated_at) \
        \SELECT gen_random_uuid(), \
        \       extracted_version.name, \
        \       extracted_version.description, \
        \       questionnaire_event_uuid_mapping.new_uuid, \
        \       extracted_version.questionnaire_uuid, \
        \       extracted_version.tenant_uuid, \
        \       user_entity.uuid, \
        \       extracted_version.created_at, \
        \       extracted_version.updated_at \
        \FROM extracted_version \
        \     LEFT JOIN questionnaire_event_uuid_mapping \
        \               ON questionnaire_event_uuid_mapping.tenant_uuid = extracted_version.tenant_uuid AND \
        \                  questionnaire_event_uuid_mapping.questionnaire_uuid = extracted_version.questionnaire_uuid AND \
        \                  questionnaire_event_uuid_mapping.old_uuid = extracted_version.event_uuid \
        \     LEFT JOIN user_entity ON extracted_version.created_by = user_entity.uuid AND \
        \                              extracted_version.tenant_uuid = user_entity.tenant_uuid \
        \;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

testVersionCount dbPool = do
  let sql =
        "DO \
        \$$ \
        \    BEGIN \
        \        IF NOT EXISTS(SELECT t1.uuid, \
        \                             t1.version_count AS version_count_original, \
        \                             t2.version_count AS version_count \
        \                      FROM (SELECT uuid, COALESCE(jsonb_array_length(versions), 0) AS version_count \
        \                            FROM questionnaire) AS t1 \
        \                           FULL OUTER JOIN \
        \                           (SELECT questionnaire_uuid, COALESCE(COUNT(*), 0) AS version_count \
        \                            FROM questionnaire_version \
        \                            GROUP BY questionnaire_uuid) AS t2 \
        \                           ON t1.uuid = t2.questionnaire_uuid \
        \                      WHERE (t1.version_count <> t2.version_count) \
        \                         OR (t1.version_count IS NULL) \
        \                         OR (t2.version_count IS NULL) AND (t1.version_count <> 0)) THEN \
        \            RAISE NOTICE 'Test PASSED: Query returned no mismatched version counts.'; \
        \        ELSE \
        \            RAISE EXCEPTION 'Test FAILED: Query returned mismatched version counts.'; \
        \        END IF; \
        \    END; \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

updateDocumentQuestionnaireEventUuid dbPool = do
  let sql =
        "UPDATE document doc \
        \SET questionnaire_event_uuid = mapping.new_uuid \
        \FROM questionnaire_event_uuid_mapping mapping \
        \WHERE doc.tenant_uuid = mapping.tenant_uuid AND doc.questionnaire_uuid = mapping.questionnaire_uuid AND mapping.old_uuid = doc.questionnaire_event_uuid;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

dropQuestionnaireEventsColumn dbPool = do
  let sql = "ALTER TABLE questionnaire DROP COLUMN events;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

dropQuestionnaireVersionColumn dbPool = do
  let sql = "ALTER TABLE questionnaire DROP COLUMN versions;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
