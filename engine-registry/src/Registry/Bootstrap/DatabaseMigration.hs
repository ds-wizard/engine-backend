module Registry.Bootstrap.DatabaseMigration where

import Control.Lens ((^.))
import Control.Monad.Logger (runStdoutLoggingT)

import LensesConfig
import qualified Registry.Database.Migration.Development.Migration as DM
import qualified Registry.Database.Migration.Production.Migration as PM
import Registry.Model.Context.AppContextHelpers
import Shared.Model.Config.Environment

runDBMigrations context =
  case context ^. appConfig . general . environment of
    Development -> runStdoutLoggingT $ runAppContextWithBaseContext DM.runMigration context
    Staging -> runStdoutLoggingT $ PM.runMigration context
    Production -> runStdoutLoggingT $ PM.runMigration context
    _ -> return ()
