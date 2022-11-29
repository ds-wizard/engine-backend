module Wizard.Database.Migration.Production.Migration_0005_documentMetadata.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta =
  MigrationMeta
    { mmNumber = 5
    , mmName = "Document metadata"
    , mmDescription = "Add workerLog and embed metadata into document table"
    }

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  addFileName dbPool
  addContentType dbPool
  addWorkerLog dbPool
  migrateFields dbPool
  removeMetadata dbPool

addFileName dbPool = do
  let sql = "ALTER TABLE document ADD file_name varchar"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

addContentType dbPool = do
  let sql = "ALTER TABLE document ADD content_type varchar"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

addWorkerLog dbPool = do
  let sql = "ALTER TABLE document ADD worker_log varchar"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

migrateFields dbPool = do
  let sql = "UPDATE document SET file_name = metadata ->> 'fileName', content_type = metadata ->> 'contentType'"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

removeMetadata dbPool = do
  let sql = "ALTER TABLE document DROP COLUMN metadata"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing
