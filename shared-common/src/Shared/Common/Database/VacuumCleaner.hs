module Shared.Common.Database.VacuumCleaner where

import Control.Monad (when)
import Control.Monad.Reader (ask)
import Data.Foldable (traverse_)
import Data.String (fromString)
import Database.PostgreSQL.Simple

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Logger
import Shared.Common.Util.String (trim)

runVacuumCleaner :: AppContextC s sc m => m ()
runVacuumCleaner = do
  context <- ask
  when
    context.serverConfig'.database'.vacuumCleaner.enabled
    (traverse_ runVacuumCleanerForTable context.serverConfig'.database'.vacuumCleaner.tables)

runVacuumCleanerForTable :: AppContextC s sc m => String -> m ()
runVacuumCleanerForTable tableName = do
  let sql = f' "VACUUM (FULL) %s" [tableName]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = execute_ conn (fromString sql)
  runDB action
  return ()
