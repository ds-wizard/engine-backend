module Shared.Bootstrap.Database where

import Control.Monad.Reader (liftIO)

import Shared.Bootstrap.Retry
import Shared.Database.Connection
import Shared.Util.Logger

connectDB serverConfigLogging serverConfigDatabase = do
  logInfo _CMP_DATABASE "connecting to the mongo database"
  dbPool <-
    liftIO $
    withRetry
      serverConfigLogging
      retryBackoff
      _CMP_DATABASE
      "failed to connect to the mongo database"
      (createDatabaseConnectionPool serverConfigDatabase)
  logInfo _CMP_DATABASE "connected"
  return dbPool
