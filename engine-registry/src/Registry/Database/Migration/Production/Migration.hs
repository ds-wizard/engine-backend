module Registry.Database.Migration.Production.Migration
  ( runMigration
  ) where

import Control.Lens ((^.))
import Database.MongoDB.Migration.Entity
import Database.MongoDB.Migration.Migration
import LensesConfig
import qualified Registry.Database.Migration.Production.Migration_0001_organization_init.Migration as M_0001
import qualified Registry.Database.Migration.Production.Migration_0002_bson_hashmap.Migration as M_0002
import qualified Registry.Database.Migration.Production.Migration_0003_package_license.Migration as M_0003
import qualified Registry.Database.Migration.Production.Migration_0004_forkOfPackageId_and_mergeCheckpointPackageId.Migration as M_0004
import qualified Registry.Database.Migration.Production.Migration_0005_add_db_indexes.Migration as M_0005
import Registry.Util.Logger

runMigration baseContext = do
  migrateDatabase (baseContext ^. pool) migrationDefinitions (logInfo _CMP_MIGRATION)
  return ()

migrationDefinitions :: [MigrationDefinition]
migrationDefinitions = [M_0001.definition, M_0002.definition, M_0003.definition, M_0004.definition, M_0005.definition]
