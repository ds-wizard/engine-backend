module Registry.Bootstrap.DatabaseMigration where

import Control.Lens ((^.))

import LensesConfig
import Registry.Bootstrap.Common
import qualified Registry.Database.Migration.Development.Migration as DM
import qualified Registry.Database.Migration.Production.Migration as PM
import Registry.Util.Logger
import Shared.Model.Config.Environment

runDBMigrations context =
  let loggingLevel = context ^. serverConfig . logging . level
   in case context ^. serverConfig . general . environment of
        Development -> runAppContextWithBaseContext DM.runMigration context
        Staging -> runLogging loggingLevel $ PM.runMigration context
        Production -> runLogging loggingLevel $ PM.runMigration context
        _ -> return ()
