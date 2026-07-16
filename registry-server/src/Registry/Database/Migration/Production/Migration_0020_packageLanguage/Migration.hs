module Registry.Database.Migration.Production.Migration_0020_packageLanguage.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 20, mmName = "Package language", mmDescription = "Add language column to knowledge_model_package"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  addPackageLanguageColumn dbPool

addPackageLanguageColumn dbPool = do
  let sql = "ALTER TABLE knowledge_model_package ADD COLUMN language varchar NOT NULL DEFAULT 'en';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
