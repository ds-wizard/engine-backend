module Wizard.Database.Migration.Production.Migration_0008_packageFk.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 8, mmName = "Package Foreign Key", mmDescription = "Remove foreign key from "}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  dropForkOfPackageIdFk dbPool
  dropMergeCheckpointPackageIdFk dbPool

dropForkOfPackageIdFk dbPool = do
  let sql = "ALTER TABLE package DROP CONSTRAINT package_fork_of_package_id_fk;"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

dropMergeCheckpointPackageIdFk dbPool = do
  let sql = "ALTER TABLE package DROP CONSTRAINT package_merge_checkpoint_package_id_fk;"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing
