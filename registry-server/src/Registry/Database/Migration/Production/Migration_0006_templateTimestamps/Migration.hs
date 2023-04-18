module Registry.Database.Migration.Production.Migration_0006_templateTimestamps.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 6, mmName = "Document Template Editor", mmDescription = "Add support for document template editor"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  addUpdatedAtToTemplate dbPool
  addCreatedAtToTemplateFile dbPool
  addUpdatedAtToTemplateFile dbPool
  addCreatedAtToTemplateAsset dbPool
  addUpdatedAtToTemplateAsset dbPool
  addPhaseToTemplate dbPool
  renameDocumentTemplateTable dbPool
  renameDocumentTemplateAssetTable dbPool
  renameDocumentTemplateFileTable dbPool
  dropDocumentTemplateRecommendedPackageId dbPool
  renameTemplateIdInAsset dbPool
  renameTemplateIdInFile dbPool

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
        \    RENAME TO document_template_organization_id_template_id_index; "
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
        \ALTER INDEX template_asset_uuid_uindex \
        \    RENAME TO document_template_asset_uindex;"
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
        \ALTER INDEX template_file_uuid_uindex \
        \    RENAME TO document_template_file_uindex; \
        \ \
        \ALTER TABLE document_template_file \
        \    RENAME CONSTRAINT template_file_template_id_fk TO document_template_file_template_id_fk;"
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
