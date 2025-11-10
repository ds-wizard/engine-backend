module Wizard.Database.Migration.Development.Document.DocumentSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Document) drop tables"
  let sql = "DROP TABLE IF EXISTS document CASCADE;"
  let action conn = execute_ conn sql
  runDB action

dropTriggers :: AppContextM Int64
dropTriggers = do
  logInfo _CMP_MIGRATION "(Trigger/Document) drop tables"
  let sql = "DROP TRIGGER IF EXISTS trigger_on_after_document_delete ON document;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  createDocumentTable
  createPersistentCommandFromDocumentDeleteFunction

createDocumentTable = do
  logInfo _CMP_MIGRATION "(Table/Document) create table"
  let sql =
        "CREATE TABLE document \
        \( \
        \    uuid                       uuid        NOT NULL, \
        \    name                       varchar     NOT NULL, \
        \    state                      varchar     NOT NULL, \
        \    durability                 varchar     NOT NULL, \
        \    questionnaire_uuid         uuid        NOT NULL, \
        \    questionnaire_event_uuid   uuid, \
        \    questionnaire_replies_hash bigint      NOT NULL, \
        \    document_template_id       varchar     NOT NULL, \
        \    format_uuid                uuid        NOT NULL, \
        \    created_by                 uuid, \
        \    retrieved_at               timestamptz, \
        \    finished_at                timestamptz, \
        \    created_at                 timestamptz NOT NULL, \
        \    file_name                  varchar, \
        \    content_type               varchar, \
        \    worker_log                 varchar, \
        \    tenant_uuid                uuid        NOT NULL, \
        \    file_size                  bigint, \
        \    CONSTRAINT document_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT document_questionnaire_uuid_fk FOREIGN KEY (questionnaire_uuid, tenant_uuid) REFERENCES questionnaire (uuid, tenant_uuid) ON DELETE SET NULL, \
        \    CONSTRAINT document_questionnaire_event_uuid_fk FOREIGN KEY (questionnaire_event_uuid, tenant_uuid) REFERENCES questionnaire_event (uuid, tenant_uuid) ON DELETE SET NULL, \
        \    CONSTRAINT document_document_template_id_fk FOREIGN KEY (document_template_id, tenant_uuid) REFERENCES document_template (id, tenant_uuid), \
        \    CONSTRAINT document_format_uuid_fk FOREIGN KEY (document_template_id, format_uuid, tenant_uuid) REFERENCES document_template_format (document_template_id, uuid, tenant_uuid), \
        \    CONSTRAINT document_created_by_fk FOREIGN KEY (created_by, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid), \
        \    CONSTRAINT document_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createPersistentCommandFromDocumentDeleteFunction = do
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
  runDB action

createTriggers :: AppContextM Int64
createTriggers = do
  logInfo _CMP_MIGRATION "(Trigger/Document) create triggers"
  let sql =
        "CREATE OR REPLACE TRIGGER trigger_on_after_document_delete \
        \    AFTER DELETE \
        \    ON document \
        \    FOR EACH ROW \
        \EXECUTE FUNCTION create_persistent_command_from_document_delete();"
  let action conn = execute_ conn sql
  runDB action
