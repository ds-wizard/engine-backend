module Wizard.Bootstrap.DatabaseMigration where

import Control.Lens ((^.))
import Control.Monad.Logger (runStdoutLoggingT)

import LensesConfig
import Shared.Model.Config.Environment
import qualified Wizard.Database.Migration.Development.Migration as DM
import qualified Wizard.Database.Migration.Production.Migration as PM
import Wizard.Model.Context.AppContextHelpers

runDBMigrations context =
  case context ^. appConfig . general . environment of
    Development -> runStdoutLoggingT $ runAppContextWithBaseContext DM.runMigration context
    Staging -> runStdoutLoggingT $ PM.runMigration context
    Production -> runStdoutLoggingT $ PM.runMigration context
    _ -> return ()
