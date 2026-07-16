module Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelLocaleSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

dropTriggers :: AppContextM Int64
dropTriggers = do
  logInfo _CMP_MIGRATION "(Trigger/KnowledgeModelLocale) drop triggers"
  let sql = "DROP TRIGGER IF EXISTS trg_knowledge_model_locale_after_delete_s3 ON knowledge_model_locale;"
  let action conn = execute_ conn sql
  runDB action

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/KnowledgeModelLocale) drop tables"
  let sql = "DROP TABLE IF EXISTS knowledge_model_locale CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/KnowledgeModelLocale) create table"
  let sql =
        "CREATE TABLE knowledge_model_locale \
        \( \
        \    uuid                         uuid        NOT NULL, \
        \    name                         varchar     NOT NULL, \
        \    code                         varchar     NOT NULL, \
        \    knowledge_model_package_uuid uuid        NOT NULL, \
        \    tenant_uuid                  uuid        NOT NULL, \
        \    created_at                   timestamptz NOT NULL, \
        \    updated_at                   timestamptz NOT NULL, \
        \    CONSTRAINT knowledge_model_locale_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT knowledge_model_locale_package_uuid_fk FOREIGN KEY (knowledge_model_package_uuid) REFERENCES knowledge_model_package (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT knowledge_model_locale_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT knowledge_model_locale_code_unique UNIQUE (knowledge_model_package_uuid, code, tenant_uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTriggers :: AppContextM Int64
createTriggers = do
  logInfo _CMP_MIGRATION "(Trigger/KnowledgeModelLocale) create triggers"
  let sql =
        "CREATE OR REPLACE TRIGGER trg_knowledge_model_locale_after_delete_s3 \
        \    AFTER DELETE \
        \    ON knowledge_model_locale \
        \    FOR EACH ROW \
        \EXECUTE FUNCTION create_persistent_command_from_entity_uuid('knowledge_model_locale', 'deleteFromS3');"
  let action conn = execute_ conn sql
  runDB action
