module Wizard.Database.Migration.Production.Migration_0007_bookReference.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Foldable (traverse_)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Data.Time
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField

import Wizard.Database.Migration.Production.Migration_0007_bookReference.Data.BookReferences

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 7, mmName = "Book Reference", mmDescription = "Add book references if missing"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql = "SELECT short_uuid FROM book_reference"
  let action conn = query_ conn (fromString sql)
  bookReferences <- liftIO $ withResource dbPool action :: LoggingT IO [[String]]
  if null bookReferences
    then insertBookReferences dbPool
    else return Nothing

insertBookReferences :: Pool Connection -> LoggingT IO (Maybe Error)
insertBookReferences dbPool = do
  now <- liftIO getCurrentTime
  traverse_ (insertBookReference dbPool now) bookReferences
  return Nothing

insertBookReference dbPool now bookReference = do
  let sql =
        "INSERT INTO book_reference (short_uuid, book_chapter, content, created_at, updated_at) VALUES (?, ?, ?, ?, ?)"
  let action conn = execute conn (fromString sql) (fmap toField bookReference ++ [toField now, toField now])
  liftIO $ withResource dbPool action
