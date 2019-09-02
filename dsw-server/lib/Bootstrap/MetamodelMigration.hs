module Bootstrap.MetamodelMigration where

import Control.Monad.Logger (runStdoutLoggingT)

import Model.Context.AppContextHelpers
import qualified Service.Migration.Metamodel.MigratorService as MM

runMetamodelMigrations context = runStdoutLoggingT $ runAppContextWithBaseContext MM.migrateCompleteDatabase context
