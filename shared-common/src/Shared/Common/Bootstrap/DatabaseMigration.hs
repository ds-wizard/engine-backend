module Shared.Common.Bootstrap.DatabaseMigration where

import Database.PostgreSQL.Migration.Migration

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger

runDBMigration baseContext prodDBMigrations runDevDBMigrations =
  if baseContext.serverConfig.database.useDevMigration
    then do
      (Right result) <- runDevDBMigrations baseContext
      return result
    else runLogging baseContext.serverConfig.logging.level $ migrateDatabase baseContext.dbPool prodDBMigrations (logInfo _CMP_MIGRATION)
