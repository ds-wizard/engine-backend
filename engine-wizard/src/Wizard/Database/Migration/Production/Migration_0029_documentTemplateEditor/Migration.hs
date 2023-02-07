module Wizard.Database.Migration.Production.Migration_0029_documentTemplateEditor.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 29, mmName = "Document Template Editor", mmDescription = "Add support for document template editor"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  addUpdatedAtToTemplate dbPool
  addCreatedAtToTemplateFile dbPool
  addUpdatedAtToTemplateFile dbPool
  addCreatedAtToTemplateAsset dbPool
  addUpdatedAtToTemplateAsset dbPool
  addPhaseToTemplate dbPool
  renameGetTemplateStateFn dbPool
  renameDocumentTemplateTable dbPool
  renameDocumentTemplateAssetTable dbPool
  renameDocumentTemplateFileTable dbPool
  renameRegistryDocumentTemplateTable dbPool
  changeToDocTmlWritePerm dbPool
  changeToDocTmlReadPerm dbPool
  dropDocumentTemplateRecommendedPackageId dbPool
  dropAppConfigTemplate dbPool
  renameTemplateIdInAsset dbPool
  renameTemplateIdInFile dbPool
  renameTemplateIdInDocument dbPool
  renameTemplateIdInQuestinonaire dbPool
  createDocumentTemplateDraftDataTable dbPool
  renameTemplateInAppLimit dbPool
  addDocumentTemplateDraftToAppLimit dbPool
  addLocaleToAppLimit dbPool

addUpdatedAtToTemplate dbPool = do
  let sql =
        "ALTER TABLE template \
        \   ADD updated_at timestamptz NOT NULL DEFAULT now();"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addCreatedAtToTemplateFile dbPool = do
  let sql =
        "ALTER TABLE template_file \
        \   ADD created_at timestamptz NOT NULL DEFAULT now();"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addUpdatedAtToTemplateFile dbPool = do
  let sql =
        "ALTER TABLE template_file \
        \   ADD updated_at timestamptz NOT NULL DEFAULT now();"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addCreatedAtToTemplateAsset dbPool = do
  let sql =
        "ALTER TABLE template_asset \
        \   ADD created_at timestamptz NOT NULL DEFAULT now();"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addUpdatedAtToTemplateAsset dbPool = do
  let sql =
        "ALTER TABLE template_asset \
        \   ADD updated_at timestamptz NOT NULL DEFAULT now();"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addPhaseToTemplate dbPool = do
  let sql =
        "ALTER TABLE template \
        \   ADD phase varchar NOT NULL DEFAULT 'ReleasedDocumentTemplatePhase';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

renameGetTemplateStateFn dbPool = do
  let sql =
        "CREATE or REPLACE FUNCTION get_template_state(remote_version varchar, local_version varchar, actual_metamodel_version int, template_metamodel_version int) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    state varchar; \
        \BEGIN \
        \    SELECT CASE \
        \               WHEN actual_metamodel_version != template_metamodel_version IS NULL THEN 'UnsupportedMetamodelVersionDocumentTemplateState' \
        \               WHEN remote_version IS NULL THEN 'UnknownDocumentTemplateState' \
        \               WHEN compare_version(remote_version, local_version) = 'LT' THEN 'UnpublishedDocumentTemplateState' \
        \               WHEN compare_version(remote_version, local_version) = 'EQ' THEN 'UpToDateDocumentTemplateState' \
        \               WHEN compare_version(remote_version, local_version) = 'GT' THEN 'OutdatedDocumentTemplateState' \
        \               END \
        \    INTO state; \
        \    RETURN state; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

renameDocumentTemplateTable dbPool = do
  let sql =
        "ALTER TABLE template \
        \    RENAME TO document_template; \
        \ \
        \ALTER INDEX template_pk \
        \    RENAME TO document_template_pk; \
        \ \
        \ALTER INDEX template_id_uindex \
        \    RENAME TO document_template_id_uindex; \
        \ \
        \ALTER INDEX template_organization_id_template_id_index \
        \    RENAME TO document_template_organization_id_template_id_index; \
        \ \
        \ALTER TABLE document_template \
        \    RENAME CONSTRAINT template_app_uuid_fk TO document_template_app_uuid_fk;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

renameDocumentTemplateAssetTable dbPool = do
  let sql =
        "ALTER TABLE template_asset \
        \    RENAME TO document_template_asset; \
        \ \
        \ALTER INDEX template_asset_pk \
        \    RENAME TO document_template_asset_pk; \
        \ \
        \ALTER INDEX template_asset_uindex \
        \    RENAME TO document_template_asset_uindex; \
        \ \
        \ALTER TABLE document_template_asset \
        \    RENAME CONSTRAINT template_asset_app_uuid_fk TO document_template_asset_app_uuid_fk; \
        \ \
        \ALTER TABLE document_template_asset \
        \    RENAME CONSTRAINT template_asset_template_id_app_uuid_fk TO document_template_asset_template_id_app_uuid_fk;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

renameDocumentTemplateFileTable dbPool = do
  let sql =
        "ALTER TABLE template_file \
        \    RENAME TO document_template_file; \
        \ \
        \ALTER INDEX template_file_pk \
        \    RENAME TO document_template_file_pk; \
        \ \
        \ALTER INDEX template_file_uindex \
        \    RENAME TO document_template_file_uindex; \
        \ \
        \ALTER TABLE document_template_file \
        \    RENAME CONSTRAINT template_file_app_uuid_fk TO document_template_file_app_uuid_fk; \
        \ \
        \ALTER TABLE document_template_file \
        \    RENAME CONSTRAINT template_file_template_id_fk TO document_template_file_template_id_fk;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

renameRegistryDocumentTemplateTable dbPool = do
  let sql =
        "ALTER TABLE registry_template \
        \    RENAME TO registry_document_template; \
        \ \
        \ALTER INDEX registry_template_pk \
        \    RENAME TO registry_document_template_pk; \
        \ \
        \ALTER INDEX registry_template_id_uindex \
        \    RENAME TO registry_document_template_id_uindex;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

changeToDocTmlWritePerm dbPool = do
  let sql =
        "UPDATE user_entity \
        \SET permissions = array_append(array_remove(permissions, 'TML_PERM'), 'DOC_TML_WRITE_PERM') \
        \WHERE role = 'admin' OR role = 'dataSteward'"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

changeToDocTmlReadPerm dbPool = do
  let sql =
        "UPDATE user_entity \
        \SET permissions = array_append(array_remove(permissions, 'DMP_PERM'), 'DOC_TML_READ_PERM')"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

dropDocumentTemplateRecommendedPackageId dbPool = do
  let sql =
        "ALTER TABLE document_template\
        \   DROP COLUMN recommended_package_id;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

dropAppConfigTemplate dbPool = do
  let sql =
        "ALTER TABLE app_config\
        \   DROP COLUMN template;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

renameTemplateIdInAsset dbPool = do
  let sql =
        "ALTER TABLE document_template_asset \
        \   RENAME template_id TO document_template_id;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

renameTemplateIdInFile dbPool = do
  let sql =
        "ALTER TABLE document_template_file\
        \   RENAME template_id TO document_template_id;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

renameTemplateIdInDocument dbPool = do
  let sql =
        "ALTER TABLE document\
        \   RENAME template_id TO document_template_id;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

renameTemplateIdInQuestinonaire dbPool = do
  let sql =
        "ALTER TABLE questionnaire\
        \   RENAME template_id TO document_template_id;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createDocumentTemplateDraftDataTable dbPool = do
  let sql =
        "CREATE TABLE document_template_draft_data \
        \ ( \
        \   document_template_id varchar not null, \
        \   questionnaire_uuid uuid, \
        \   format_uuid uuid, \
        \   app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \     CONSTRAINT document_template_draft_data_app_uuid_fk \
        \       REFERENCES app, \
        \   created_at             timestamp with time zone not null, \
        \   updated_at             timestamp with time zone not null \
        \ ); \
        \  \
        \ALTER TABLE document_template_draft_data \
        \   ADD CONSTRAINT document_template_draft_data_document_template_id_fk \
        \      FOREIGN KEY (document_template_id, app_uuid) REFERENCES document_template (id, app_uuid); \
        \  \
        \CREATE UNIQUE INDEX document_template_draft_data_document_template_id_uindex \
        \   ON document_template_draft_data (document_template_id, app_uuid); \
        \  \
        \ALTER TABLE document_template_draft_data \
        \   ADD CONSTRAINT document_template_draft_data_pk \
        \      PRIMARY KEY (document_template_id, app_uuid);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

renameTemplateInAppLimit dbPool = do
  let sql =
        "ALTER TABLE app_limit \
        \   RENAME templates TO document_templates;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addDocumentTemplateDraftToAppLimit dbPool = do
  let sql =
        "ALTER TABLE app_limit \
        \   ADD document_template_drafts int;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addLocaleToAppLimit dbPool = do
  let sql =
        "ALTER TABLE app_limit \
        \   ADD locales int;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
