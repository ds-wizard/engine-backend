module Registry.Bootstrap.Database where

import Control.Monad.Reader (liftIO)

import Registry.Bootstrap.Retry
import Registry.Constant.Component
import Registry.Database.Connection
import Registry.Util.Logger

connectDB serverConfig = do
  logInfo $ msg _CMP_DATABASE "connecting to the database"
  dbPool <-
    liftIO $
    withRetry retryBackoff _CMP_DATABASE "failed to connect to the database" (createDatabaseConnectionPool serverConfig)
  logInfo $ msg _CMP_DATABASE "connected"
  return dbPool
