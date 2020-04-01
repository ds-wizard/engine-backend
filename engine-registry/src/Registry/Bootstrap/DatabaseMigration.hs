module Registry.Bootstrap.DatabaseMigration where

import Control.Lens ((^.))
import Control.Monad.Logger (runStdoutLoggingT)

import LensesConfig
import Registry.Bootstrap.Common
import qualified Registry.Database.Migration.Development.Migration as DM
import qualified Registry.Database.Migration.Production.Migration as PM
import Shared.Model.Config.Environment

runDBMigrations context =
  case context ^. serverConfig . general . environment of
    Development -> runAppContextWithBaseContext DM.runMigration context
    Staging -> runStdoutLoggingT $ PM.runMigration context
    Production -> runStdoutLoggingT $ PM.runMigration context
    _ -> return ()
