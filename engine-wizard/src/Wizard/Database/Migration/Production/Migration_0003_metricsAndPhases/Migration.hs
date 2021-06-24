module Wizard.Database.Migration.Production.Migration_0003_metricsAndPhases.Migration
  ( definition
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
    {mmNumber = 3, mmName = "Metrics and Phases", mmDescription = "Move metrics and phases into Knowledge Model"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = dropMetricTable dbPool

dropMetricTable dbPool = do
  let sql = "DROP TABLE metric;"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing
