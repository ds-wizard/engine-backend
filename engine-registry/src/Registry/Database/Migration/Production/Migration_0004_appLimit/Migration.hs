module Registry.Database.Migration.Production.Migration_0004_appLimit.Migration (
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
    { mmNumber = 4
    , mmName = "App Limit"
    , mmDescription = "Add file size and internal flag for persistent Command"
    }

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  addFileSizeToTemplateAsset dbPool
  addInternalFlagToPersistentCommand dbPool

addFileSizeToTemplateAsset dbPool = do
  let sql =
        "ALTER TABLE template_asset \
        \     ADD file_size bigint not null default 0;"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

addInternalFlagToPersistentCommand dbPool = do
  let sql =
        "ALTER TABLE persistent_command \
        \ADD column internal bool not null default true"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing
