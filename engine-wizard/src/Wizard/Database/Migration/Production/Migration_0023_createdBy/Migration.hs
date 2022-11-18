module Wizard.Database.Migration.Production.Migration_0023_createdBy.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta =
  MigrationMeta
    { mmNumber = 23
    , mmName = "Questionnaire creatorUuid"
    , mmDescription = "Rename creatorUuid to createdBy in Questionnaire"
    }

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  renameBranch dbPool
  renameQuestionnaire dbPool

renameBranch dbPool = do
  let sql =
        "ALTER TABLE branch \
        \    RENAME COLUMN owner_uuid TO created_by;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

renameQuestionnaire dbPool = do
  let sql =
        "ALTER TABLE questionnaire \
        \    RENAME COLUMN creator_uuid TO created_by;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
