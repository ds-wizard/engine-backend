module Wizard.Bootstrap.MetamodelMigration where

import Control.Monad.Logger (runStdoutLoggingT)

import Wizard.Model.Context.AppContextHelpers
import qualified Wizard.Service.Migration.Metamodel.MigratorService as MM

runMetamodelMigrations context = runStdoutLoggingT $ runAppContextWithBaseContext MM.migrateCompleteDatabase context
