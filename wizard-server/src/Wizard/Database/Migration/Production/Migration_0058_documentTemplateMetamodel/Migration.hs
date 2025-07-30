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
  updateMetamodelVersionForDocumentTemplateEditor dbPool
  createSemVer2TupleType dbPool
  changeMetamodelVersionColumnTypeInDocumentTemplate dbPool
  addValueRawColumnToQuestionnaireEventTable dbPool
  createKnowledgeModelSecretTable dbPool

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
