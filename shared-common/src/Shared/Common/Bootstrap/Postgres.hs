module Shared.Common.Bootstrap.Postgres where

import Control.Monad.Reader (liftIO)

import Shared.Common.Bootstrap.Retry
import Shared.Common.Database.Connection
import Shared.Common.Util.Logger

connectPostgresDB serverConfigLogging serverConfigDatabase = do
  logInfo _CMP_DATABASE "connecting to the postgres database"
  dbPool <-
    liftIO $
      withRetry
        serverConfigLogging
        retryBackoff
        _CMP_DATABASE
        "failed to connect to the postgres database"
        (createDatabaseConnectionPool serverConfigDatabase)
  logInfo _CMP_DATABASE "connected"
  return dbPool
