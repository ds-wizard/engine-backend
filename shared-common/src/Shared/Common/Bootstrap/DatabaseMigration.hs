module Shared.Common.Bootstrap.DatabaseMigration where

import Database.PostgreSQL.Migration.Migration

import Shared.Common.Constant.Component
import Shared.Common.Model.Config.Environment
import Shared.Common.Util.Logger

runDBMigration baseContext prodDBMigrations runDevDBMigrations =
  case baseContext.serverConfig.general.environment of
    Development -> do
      (Right result) <- runDevDBMigrations baseContext
      return result
    Production -> runLogging baseContext.serverConfig.logging.level $ migrateDatabase baseContext.dbPool prodDBMigrations (logInfo _CMP_MIGRATION)
