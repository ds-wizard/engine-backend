module Wizard.Bootstrap.DatabaseMigration where

import Control.Lens ((^.))
import Control.Monad.Logger (runStdoutLoggingT)

import LensesConfig
import Shared.Model.Config.Environment
import Wizard.Bootstrap.Common
import qualified Wizard.Database.Migration.Development.Migration as DM
import qualified Wizard.Database.Migration.Production.Migration as PM

runDBMigrations context =
  case context ^. serverConfig . general . environment of
    Development -> runAppContextWithBaseContext DM.runMigration context
    Staging -> runStdoutLoggingT $ PM.runMigration context
    Production -> runStdoutLoggingT $ PM.runMigration context
    _ -> return ()
