module Database.Migration.Migrator.MigratorMigration where

import Control.Lens ((^.))
import Control.Monad.Logger (logInfo)
import Control.Monad.Reader (liftIO)

import Database.DAO.Migrator.MigratorDAO
import LensesConfig
import Model.Context.AppContext

runMigration appContext = do
  $(logInfo) "MIGRATION (Migrator/Migrator): started"
  let context = appContext ^. oldContext
  liftIO $ deleteMigratorStates context
  $(logInfo) "MIGRATION (Migrator/Migrator): ended"
