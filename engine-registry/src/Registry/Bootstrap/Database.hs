module Registry.Bootstrap.Database where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)

import LensesConfig
import Registry.Bootstrap.Retry
import Registry.Util.Logger
import Shared.Database.Connection

connectDB serverConfig = do
  logInfo _CMP_DATABASE "connecting to the database"
  dbPool <-
    liftIO $
    withRetry
      serverConfig
      retryBackoff
      _CMP_DATABASE
      "failed to connect to the database"
      (createDatabaseConnectionPool (serverConfig ^. database))
  logInfo _CMP_DATABASE "connected"
  return dbPool
