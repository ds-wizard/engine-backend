module Bootstrap.Database where

import Control.Monad.Reader (liftIO)

import Bootstrap.Retry
import Constant.Component
import Database.Connection
import Util.Logger

connectDB appConfig = do
  logInfo $ msg _CMP_DATABASE "connecting to the database"
  dbPool <-
    liftIO $
    withRetry retryBackoff _CMP_DATABASE "failed to connect to the database" (createDatabaseConnectionPool appConfig)
  logInfo $ msg _CMP_DATABASE "connected"
  return dbPool
