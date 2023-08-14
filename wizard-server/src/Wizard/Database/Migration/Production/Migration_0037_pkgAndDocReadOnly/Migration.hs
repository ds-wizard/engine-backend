module Wizard.Database.Migration.Production.Migration_0037_pkgAndDocReadOnly.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 37, mmName = "Package & Document Template Read Only", mmDescription = "Add nonEditable to package and document template"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  addToPackage dbPool
  addToDocumentTemplate dbPool

addToPackage dbPool = do
  let sql =
        "ALTER TABLE package\
        \   ADD non_editable BOOLEAN NOT NULL DEFAULT false;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addToDocumentTemplate dbPool = do
  let sql =
        "ALTER TABLE document_template\
        \   ADD non_editable BOOLEAN NOT NULL DEFAULT false;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
