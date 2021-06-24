module Wizard.Database.Migration.Production.Migration
  ( runMigration
  ) where

import Control.Lens ((^.))
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Migration.Migration
import LensesConfig
import qualified Wizard.Database.Migration.Production.Migration_0001_init.Migration as M_0001
import qualified Wizard.Database.Migration.Production.Migration_0002_projectTemplate.Migration as M_0002
import qualified Wizard.Database.Migration.Production.Migration_0003_metricsAndPhases.Migration as M_0003
import Wizard.Util.Logger

runMigration baseContext = do
  migrateDatabase (baseContext ^. dbPool) migrationDefinitions (logInfo _CMP_MIGRATION)
  return ()

migrationDefinitions :: [MigrationDefinition]
migrationDefinitions = [M_0001.definition, M_0002.definition, M_0003.definition]
