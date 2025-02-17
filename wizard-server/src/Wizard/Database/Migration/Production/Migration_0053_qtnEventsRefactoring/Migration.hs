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
  regenerateQuestionnaireEventUuid dbPool
  createQuestionnaireEventTable dbPool
  insertQuestionnaireEvents dbPool
  createQuestionnaireEventIndex dbPool
  testEventCount dbPool
  testValueCount dbPool
  removeEventsColumnFromQuestionnaire dbPool
  changePasswordHashForSystemUser dbPool
  createExternalLinkTable dbPool

regenerateQuestionnaireEventUuid dbPool = do
  let sql =
        "WITH json_data AS (  \
        \    SELECT events  \
        \    FROM questionnaire  \
        \),  \
        \uuid_values AS (  \
        \    SELECT (jsonb_array_elements(events))->> 'uuid' AS uuid_value  \
        \    FROM json_data  \
        \),  \
        \non_unique_uuids AS (  \
        \    SELECT uuid_value  \
        \    FROM uuid_values  \
        \    GROUP BY uuid_value  \
        \    HAVING COUNT(*) > 1  \
        \),  \
        \expanded AS (  \
        \    SELECT  \
        \        uuid,  \
        \        jsonb_agg(  \
        \            CASE  \
        \                WHEN elem->>'uuid' IN (SELECT uuid_value FROM non_unique_uuids) THEN  \
        \                    jsonb_set(elem, '{uuid}', to_jsonb(gen_random_uuid()::text))  \
        \                ELSE  \
        \                    elem  \
        \            END  \
        \        ) AS updated_json  \
        \    FROM questionnaire,  \
        \    LATERAL jsonb_array_elements(events) AS elem  \
        \    GROUP BY uuid  \
        \)  \
        \UPDATE questionnaire  \
        \SET events = expanded.updated_json  \
        \FROM expanded  \
        \WHERE questionnaire.uuid = expanded.uuid;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createQuestionnaireEventTable dbPool = do
  let sql =
        "CREATE TYPE event_type AS ENUM ('ClearReplyEvent', 'SetReplyEvent', 'SetLabelsEvent', 'SetPhaseEvent'); \
        \CREATE TYPE value_type AS ENUM ('IntegrationReply', 'AnswerReply', 'MultiChoiceReply', 'ItemListReply', 'StringReply', 'ItemSelectReply', 'FileReply'); \
        \ \
        \CREATE TABLE IF NOT EXISTS questionnaire_event ( \
        \    uuid uuid PRIMARY KEY, \
        \    event_type event_type NOT NULL, \
        \    path text, \
        \    created_at timestamp with time zone NOT NULL, \
        \    created_by uuid, \
        \    questionnaire_uuid uuid NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    value_type value_type, \
        \    value text[], \
        \    value_id text, \
        \    CONSTRAINT questionnaire_uuid_tenant_uuid_fk FOREIGN KEY (questionnaire_uuid, tenant_uuid) references questionnaire(uuid, tenant_uuid) \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

insertQuestionnaireEvents dbPool = do
  let sql =
        "INSERT INTO questionnaire_event (uuid, event_type, path, created_at, created_by, questionnaire_uuid, tenant_uuid, value_type, value, value_id) \
        \SELECT \
        \    (elem->>'uuid')::uuid AS uuid, \
        \    (elem->>'type')::event_type AS event_type, \
        \    elem->>'path' AS path, \
        \    (elem->>'createdAt')::TIMESTAMP WITH TIME ZONE AS created_at, \
        \    (elem->>'createdBy')::uuid AS created_by, \
        \    questionnaire.uuid AS questionnaire_uuid, \
        \    questionnaire.tenant_uuid AS tenant_uuid, \
        \    (elem->'value'->>'type')::value_type AS value_type, \
        \    CASE \
        \      WHEN elem->'value'->>'type' = 'IntegrationReply' THEN \
        \          CASE \
        \              WHEN elem->'value'->'value'->>'value' = '' THEN '{\"\"}'::text[] \
        \              WHEN elem->'value'->'value'->>'value' IS NULL THEN NULL \
        \              ELSE array_agg(elem->'value'->'value'->>'value') \
        \          END \
        \      WHEN elem->'value'->>'type' IN ('AnswerReply', 'StringReply', 'ItemSelectReply', 'FileReply') THEN \
        \          CASE \
        \              WHEN elem->'value'->>'value' = '' THEN '{\"\"}'::text[] \
        \              WHEN elem->'value'->>'value' IS NULL THEN NULL \
        \              ELSE array_agg(elem->'value'->>'value') \
        \          END \
        \    END AS value, \
        \    elem->'value'->'value'->>'id' AS value_id \
        \FROM \
        \    questionnaire \
        \LEFT JOIN LATERAL jsonb_array_elements(events) AS elem ON TRUE \
        \WHERE \
        \  elem->'value' IS NOT NULL AND \
        \  elem->'value'->>'type' IN ('IntegrationReply', 'AnswerReply', 'StringReply', 'ItemSelectReply', 'FileReply') \
        \GROUP BY questionnaire.uuid, questionnaire.tenant_uuid, elem \
        \; \
        \ \
        \INSERT INTO questionnaire_event (uuid, event_type, path, created_at, created_by, questionnaire_uuid, tenant_uuid, value_type, value) \
        \SELECT \
        \    (elem->>'uuid')::uuid AS uuid, \
        \    (elem->>'type')::event_type AS event_type, \
        \    elem->>'path' AS path, \
        \    (elem->>'createdAt')::TIMESTAMP WITH TIME ZONE AS created_at, \
        \    (elem->>'createdBy')::uuid AS created_by, \
        \    questionnaire.uuid AS questionnaire_uuid, \
        \    questionnaire.tenant_uuid AS tenant_uuid, \
        \    (elem->'value'->>'type')::value_type AS event_type, \
        \    CASE \
        \      WHEN jsonb_array_length(elem->'value'->'value') = 0 THEN '{}'::text[] \
        \      ELSE array_agg(v ORDER BY o) \
        \    END AS value \
        \FROM \
        \  questionnaire \
        \LEFT JOIN LATERAL jsonb_array_elements(events) AS elem ON TRUE \
        \LEFT JOIN LATERAL jsonb_array_elements_text(elem->'value'->'value') WITH ORDINALITY AS values(v, o) ON TRUE \
        \WHERE \
        \  elem->'value' IS NOT NULL AND \
        \  elem->'value'->>'type' = 'MultiChoiceReply' \
        \GROUP BY questionnaire.uuid, questionnaire.tenant_uuid, elem \
        \; \
        \ \
        \INSERT INTO questionnaire_event (uuid, event_type, path, created_at, created_by, questionnaire_uuid, tenant_uuid, value_type, value) \
        \SELECT \
        \    (elem->>'uuid')::uuid AS uuid, \
        \    (elem->>'type')::event_type AS event_type, \
        \    elem->>'path' AS path, \
        \    (elem->>'createdAt')::TIMESTAMP WITH TIME ZONE AS created_at, \
        \    (elem->>'createdBy')::uuid AS created_by, \
        \    questionnaire.uuid AS questionnaire_uuid, \
        \    questionnaire.tenant_uuid AS tenant_uuid, \
        \    (elem->'value'->>'type')::value_type AS value_type, \
        \    CASE \
        \      WHEN jsonb_array_length(elem->'value'->'value') = 0 THEN '{}'::text[] \
        \      ELSE array_agg(v ORDER BY o) \
        \    END AS value \
        \FROM \
        \  questionnaire \
        \LEFT JOIN LATERAL jsonb_array_elements(events) AS elem ON TRUE \
        \LEFT JOIN LATERAL jsonb_array_elements_text(elem->'value'->'value') WITH ORDINALITY AS values(v, o) ON TRUE \
        \WHERE \
        \  elem->'value' IS NOT NULL AND \
        \  elem->'value'->>'type' = 'ItemListReply' \
        \GROUP BY questionnaire.uuid, questionnaire.tenant_uuid, elem \
        \; \
        \ \
        \INSERT INTO questionnaire_event (uuid, event_type, path, created_at, created_by, questionnaire_uuid, tenant_uuid, value) \
        \SELECT \
        \  (elem->>'uuid')::uuid AS uuid, \
        \  (elem->>'type')::event_type AS event_type, \
        \  elem->>'path' AS path, \
        \  (elem->>'createdAt')::TIMESTAMP WITH TIME ZONE AS created_at, \
        \  (elem->>'createdBy')::uuid AS created_by, \
        \  questionnaire.uuid AS questionnaire_uuid, \
        \  questionnaire.tenant_uuid AS tenant_uuid, \
        \  CASE \
        \      WHEN jsonb_array_length(elem->'value') = 0 THEN '{}'::text[] \
        \      ELSE array_agg(v ORDER BY o) \
        \  END AS value \
        \FROM \
        \  questionnaire \
        \LEFT JOIN LATERAL jsonb_array_elements(events) AS elem ON TRUE \
        \LEFT JOIN LATERAL jsonb_array_elements_text(elem->'value') WITH ORDINALITY AS values(v, o) ON TRUE \
        \WHERE \
        \  elem->'value' IS NOT NULL AND \
        \  elem->>'type' = 'SetLabelsEvent' \
        \GROUP BY questionnaire.uuid, questionnaire.tenant_uuid, elem \
        \; \
        \ \
        \INSERT INTO questionnaire_event (uuid, event_type, path, created_at, created_by, questionnaire_uuid, tenant_uuid, value) \
        \SELECT \
        \  (elem->>'uuid')::uuid AS uuid, \
        \  (elem->>'type')::event_type AS event_type, \
        \  elem->>'path' AS path, \
        \  (elem->>'createdAt')::TIMESTAMP WITH TIME ZONE AS created_at, \
        \  (elem->>'createdBy')::uuid AS created_by, \
        \  questionnaire.uuid AS questionnaire_uuid, \
        \  questionnaire.tenant_uuid AS tenant_uuid, \
        \  CASE \
        \      WHEN elem->>'phaseUuid' IS NULL THEN NULL \
        \      ELSE array_agg(elem->>'phaseUuid') \
        \  END AS value \
        \FROM \
        \    questionnaire \
        \LEFT JOIN LATERAL jsonb_array_elements(events) AS elem ON TRUE \
        \WHERE \
        \  elem->>'type' = 'SetPhaseEvent' \
        \GROUP BY questionnaire.uuid, questionnaire.tenant_uuid, elem \
        \; \
        \ \
        \INSERT INTO questionnaire_event (uuid, event_type, path, created_at, created_by, questionnaire_uuid, tenant_uuid) \
        \SELECT \
        \  (elem->>'uuid')::uuid AS uuid, \
        \  (elem->>'type')::event_type AS event_type, \
        \  elem->>'path' AS path, \
        \  (elem->>'createdAt')::TIMESTAMP WITH TIME ZONE AS created_at, \
        \  (elem->>'createdBy')::uuid AS created_by, \
        \  questionnaire.uuid AS questionnaire_uuid, \
        \  questionnaire.tenant_uuid AS tenant_uuid \
        \FROM \
        \    questionnaire \
        \LEFT JOIN LATERAL jsonb_array_elements(events) AS elem ON TRUE \
        \WHERE \
        \  elem->>'type' = 'ClearReplyEvent' \
        \GROUP BY questionnaire.uuid, questionnaire.tenant_uuid, elem;"
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
        "DO $$ \
        \BEGIN \
        \    IF NOT EXISTS( \
        \        SELECT \
        \            t1.uuid, \
        \            t1.event_count AS event_count_original, \
        \             t2.event_count AS event_count \
        \        FROM \
        \            (SELECT uuid, COALESCE(jsonb_array_length(events), 0) AS event_count FROM questionnaire) AS t1 \
        \        FULL OUTER JOIN \
        \            (SELECT questionnaire_uuid, COALESCE(COUNT(*), 0) AS event_count FROM questionnaire_event GROUP BY questionnaire_uuid) AS t2 \
        \        ON t1.uuid = t2.questionnaire_uuid \
        \        WHERE (t1.event_count <> t2.event_count) \
        \            OR (t1.event_count IS NULL) \
        \            OR (t2.event_count IS NULL) AND (t1.event_count <> 0) \
        \    ) THEN \
        \        RAISE NOTICE 'Test PASSED: Query returned no mismatched event counts.'; \
        \    ELSE \
        \        RAISE EXCEPTION 'Test FAILED: Query returned mismatched event counts.'; \
        \    END IF; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

testValueCount dbPool = do
  let sql =
        "DO $$ \
        \BEGIN \
        \    IF NOT EXISTS( \
        \        SELECT \
        \            t1.uuid, \
        \            t1.phase_uuid, \
        \            t1.value_count AS value_count_original, \
        \            t2.value_count AS value_count, \
        \            t2.event_type AS event_type \
        \        FROM \
        \            (SELECT \
        \            (elem->>'uuid')::uuid AS uuid, \
        \            (elem->>'phaseUuid')::uuid AS phase_uuid, \
        \            SUM( \
        \                CASE \
        \                    WHEN jsonb_typeof(elem->'value') = 'array' THEN jsonb_array_length(elem->'value') \
        \                    WHEN jsonb_typeof(elem->'value') = 'string' THEN 1 \
        \                    WHEN jsonb_typeof(elem->'value') = 'object' AND jsonb_typeof(elem->'value'->'value') = 'object' THEN 1 \
        \                    WHEN jsonb_typeof(elem->'value') = 'object' AND jsonb_typeof(elem->'value'->'value') = 'string' THEN 1 \
        \                    WHEN jsonb_typeof(elem->'value') = 'object' AND jsonb_typeof(elem->'value'->'value') = 'array' THEN jsonb_array_length(elem->'value'->'value') \
        \                    ELSE 0 \
        \                END \
        \            ) AS value_count \
        \            FROM \
        \                questionnaire, \
        \                jsonb_array_elements(events) AS elem \
        \            GROUP BY elem->>'uuid', elem->>'phaseUuid') AS t1 \
        \        FULL OUTER JOIN \
        \            (SELECT uuid, event_type, array_length(value, 1) AS value_count FROM questionnaire_event GROUP BY uuid) AS t2 \
        \        ON \
        \            t1.uuid = t2.uuid \
        \        WHERE \
        \            (t2.event_type = 'ClearReplyEvent' AND t1.value_count <> t2.value_count) \
        \        OR  (t2.event_type = 'SetPhaseEvent' AND t1.phase_uuid IS NULL AND t2.value_count <> 0) \
        \        OR  (t2.event_type = 'SetPhaseEvent' AND t1.phase_uuid IS NOT NULL AND t2.value_count <> 1) \
        \        OR  (t2.event_type = 'SetLabelsEvent' AND t1.value_count <> t2.value_count) \
        \        OR  (t2.event_type = 'SetReplyEvent' AND t1.value_count <> t2.value_count) \
        \    ) THEN \
        \        RAISE NOTICE 'Test PASSED: Query returned no mismatched value counts.'; \
        \    ELSE \
        \        RAISE EXCEPTION 'Test FAILED: Query returned mismatched value counts.'; \
        \    END IF; \
        \END; \
        \$$;"

  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

removeEventsColumnFromQuestionnaire dbPool = do
  let sql = "ALTER TABLE questionnaire DROP COLUMN events;"
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
