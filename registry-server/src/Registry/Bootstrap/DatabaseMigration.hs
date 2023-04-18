module Registry.Bootstrap.DatabaseMigration where

import Registry.Bootstrap.Common
import qualified Registry.Database.Migration.Development.Migration as DM
import qualified Registry.Database.Migration.Production.Migration as PM
import Registry.Model.Config.ServerConfig
import Registry.Model.Context.BaseContext
import Shared.Common.Model.Config.Environment
import Shared.Common.Model.Config.ServerConfig

runDBMigrations :: BaseContext -> IO (Maybe String)
runDBMigrations context =
  case context.serverConfig.general.environment of
    Development -> runAppContextWithBaseContext DM.runMigration context
    Production -> PM.runMigration context
