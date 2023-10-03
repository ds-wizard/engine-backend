module Registry.Database.Migration.Production.Migration (
  runMigration,
) where

import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Migration.Migration

import qualified Registry.Database.Migration.Production.Migration_0001_init.Migration as M_0001
import qualified Registry.Database.Migration.Production.Migration_0002_app.Migration as M_0002
import qualified Registry.Database.Migration.Production.Migration_0003_persistentCommand.Migration as M_0003
import qualified Registry.Database.Migration.Production.Migration_0004_appLimit.Migration as M_0004
import qualified Registry.Database.Migration.Production.Migration_0005_locale.Migration as M_0005
import qualified Registry.Database.Migration.Production.Migration_0006_templateTimestamps.Migration as M_0006
import qualified Registry.Database.Migration.Production.Migration_0007_component.Migration as M_0007
import qualified Registry.Database.Migration.Production.Migration_0008_unification.Migration as M_0008
import qualified Registry.Database.Migration.Production.Migration_0009_persistentCommandDestination.Migration as M_0009
import qualified Registry.Database.Migration.Production.Migration_0010_pkgAndDocReadOnly.Migration as M_0010
import qualified Registry.Database.Migration.Production.Migration_0011_traceUuid.Migration as M_0011
import Shared.Common.Util.Logger

runMigration baseContext = do
  let loggingLevel = baseContext.serverConfig.logging.level
  runLogging loggingLevel $ migrateDatabase baseContext.dbPool migrationDefinitions (logInfo _CMP_MIGRATION)

migrationDefinitions :: [MigrationDefinition]
migrationDefinitions =
  [ M_0001.definition
  , M_0002.definition
  , M_0003.definition
  , M_0004.definition
  , M_0005.definition
  , M_0006.definition
  , M_0007.definition
  , M_0008.definition
  , M_0009.definition
  , M_0010.definition
  , M_0011.definition
  ]
