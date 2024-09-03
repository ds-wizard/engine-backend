module Wizard.Database.Migration.Production.Migration_0007_bookReference.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 7, mmName = "Book Reference", mmDescription = "Add book references if missing"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql = "SELECT short_uuid FROM book_reference"
  let action conn = query_ conn (fromString sql)
  bookReferences <- liftIO $ withResource dbPool action :: LoggingT IO [[String]]
  return Nothing
