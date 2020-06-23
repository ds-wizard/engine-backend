module Wizard.Bootstrap.DatabaseMigration where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Config.Environment
import Wizard.Bootstrap.Common
import qualified Wizard.Database.Migration.Development.Migration as DM
import qualified Wizard.Database.Migration.Production.Migration as PM
import Wizard.Util.Logger

runDBMigrations context =
  let loggingLevel = context ^. serverConfig . logging . level
   in case context ^. serverConfig . general . environment of
        Development -> runAppContextWithBaseContext DM.runMigration context
        Staging -> runLogging loggingLevel $ PM.runMigration context
        Production -> runLogging loggingLevel $ PM.runMigration context
        _ -> return ()
