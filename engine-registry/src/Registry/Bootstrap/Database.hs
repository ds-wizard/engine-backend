module Registry.Bootstrap.Database where

import Control.Monad.Reader (liftIO)

import Registry.Bootstrap.Retry
import Registry.Constant.Component
import Registry.Util.Logger
import Shared.Database.Connection

connectDB serverConfig = do
  logInfo _CMP_DATABASE "connecting to the database"
  dbPool <-
    liftIO $
    withRetry retryBackoff _CMP_DATABASE "failed to connect to the database" (createDatabaseConnectionPool serverConfig)
  logInfo _CMP_DATABASE "connected"
  return dbPool
