module Bootstrap.DatabaseMigration where

import Control.Lens ((^.))
import Control.Monad.Logger (runStdoutLoggingT)

import qualified Database.Migration.Development.Migration as DM
import qualified Database.Migration.Production.Migration as PM
import LensesConfig
import Model.Config.Environment
import Model.Context.AppContextHelpers

runDBMigrations context =
  case context ^. appConfig . general . environment of
    Development -> runStdoutLoggingT $ runAppContextWithBaseContext DM.runMigration context
    Staging -> runStdoutLoggingT $ PM.runMigration context
    Production -> runStdoutLoggingT $ PM.runMigration context
    _ -> return ()
