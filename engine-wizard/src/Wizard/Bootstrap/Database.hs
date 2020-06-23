module Wizard.Bootstrap.Database where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)

import LensesConfig
import Shared.Database.Connection
import Wizard.Bootstrap.Retry
import Wizard.Constant.Component
import Wizard.Util.Logger

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
