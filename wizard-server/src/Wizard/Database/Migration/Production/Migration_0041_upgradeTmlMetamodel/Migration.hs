module Wizard.Database.Migration.Production.Migration_0041_upgradeTmlMetamodel.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 41, mmName = "Document Template Metamodel 12", mmDescription = "Upgrade Document Template Metamodel to version 12"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql = "UPDATE document_template SET metamodel_version = 12 WHERE phase = 'DraftDocumentTemplatePhase';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
