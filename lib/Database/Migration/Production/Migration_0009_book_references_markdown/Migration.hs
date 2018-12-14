module Database.Migration.Production.Migration_0009_book_references_markdown.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Time
import Database.MongoDB
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

import Database.Migration.Production.Migration_0009_book_references_markdown.Data.BookReferences

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 9, mmName = "Book References Markdown", mmDescription = ""}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  now <- liftIO getCurrentTime
  let action = updateMany "bookReferences" (bookReferencesMD now)
  runMongoDBPoolDef action dbPool
  return Nothing
