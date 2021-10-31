module Wizard.Bootstrap.DatabaseMigration where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Config.Environment
import Wizard.Bootstrap.Common
import qualified Wizard.Database.Migration.Development.Migration as DM
import qualified Wizard.Database.Migration.Production.Migration as PM
import Wizard.Model.Context.BaseContext

runDBMigrations :: BaseContext -> IO (Maybe String)
runDBMigrations context =
  case context ^. serverConfig . general . environment of
    Development -> runAppContextWithBaseContext DM.runMigration context
    Production -> PM.runMigration context
