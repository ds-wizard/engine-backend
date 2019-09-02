module Database.Migration.Production.Migration_0003_book_references_init.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Time
import Database.MongoDB
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

import Database.Migration.Production.Migration_0003_book_references_init.Data.BookReferences

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 3, mmName = "Book References Init", mmDescription = ""}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  now <- liftIO getCurrentTime
  let action = insertMany "bookReferences" (bookReferences now)
  runMongoDBPoolDef action dbPool
  return Nothing
