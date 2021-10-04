module Wizard.Database.Migration.Production.Migration_0009_adminOperations.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 9, mmName = "Admin Operations", mmDescription = "Add ADMIN_PERM to admin users"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql = "UPDATE user_entity set permissions = permissions || '{ADMIN_PERM}' WHERE role = 'admin'"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing
