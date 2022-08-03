module Wizard.Database.DAO.Limit.AppLimitDAO where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Reader (asks, liftIO)
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import LensesConfig
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Limit.AppLimit ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Limit.AppLimit

entityName = "app_limit"

findAppLimits :: AppContextM [AppLimit]
findAppLimits = createFindEntitiesFn entityName

findAppLimitById :: String -> AppContextM AppLimit
findAppLimitById uuid = createFindEntityByFn entityName [("uuid", uuid)]

findCurrentAppLimit :: AppContextM AppLimit
findCurrentAppLimit = do
  appUuid <- asks _appContextAppUuid
  findAppLimitById (U.toString appUuid)

insertAppLimit :: AppLimit -> AppContextM Int64
insertAppLimit = createInsertFn entityName

updateAppLimitById :: AppLimit -> AppContextM AppLimit
updateAppLimitById appLimit = do
  now <- liftIO getCurrentTime
  let updatedAppLimit = appLimit & updatedAt .~ now
  let sql =
        fromString
          "UPDATE app_limit SET uuid = ?, users = ?, active_users = ?, knowledge_models = ?, branches = ?, templates = ?, questionnaires = ?, documents =?, storage = ?, created_at = ?, updated_at = ? WHERE uuid = ?"
  let params = toRow updatedAppLimit ++ [toField $ updatedAppLimit ^. uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  return updatedAppLimit

deleteAppLimits :: AppContextM Int64
deleteAppLimits = createDeleteEntitiesFn entityName
