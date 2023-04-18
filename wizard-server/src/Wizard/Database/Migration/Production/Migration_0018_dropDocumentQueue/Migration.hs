module Wizard.Database.Migration.Production.Migration_0018_dropDocumentQueue.Migration (
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
    { mmNumber = 18
    , mmName = "Drop document queue"
    , mmDescription = "Replace document queue with persistent command"
    }

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql = "DROP TABLE IF EXISTS document_queue"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing
