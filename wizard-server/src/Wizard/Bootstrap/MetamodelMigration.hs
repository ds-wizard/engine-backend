module Wizard.Bootstrap.MetamodelMigration where

import Wizard.Bootstrap.Common
import qualified Wizard.Service.Migration.Metamodel.MigratorService as MM

runMetamodelMigrations = runAppContextWithBaseContext MM.migrateCompleteDatabase
