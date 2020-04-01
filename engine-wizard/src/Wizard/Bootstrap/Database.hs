module Wizard.Bootstrap.Database where

import Control.Monad.Reader (liftIO)

import Wizard.Bootstrap.Retry
import Wizard.Constant.Component
import Wizard.Database.Connection
import Wizard.Util.Logger

connectDB serverConfig = do
  logInfo _CMP_DATABASE "connecting to the database"
  dbPool <-
    liftIO $
    withRetry retryBackoff _CMP_DATABASE "failed to connect to the database" (createDatabaseConnectionPool serverConfig)
  logInfo _CMP_DATABASE "connected"
  return dbPool
