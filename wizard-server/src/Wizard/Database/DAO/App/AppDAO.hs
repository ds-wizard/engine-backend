module Wizard.Database.DAO.App.AppDAO where

import Control.Monad.Reader (liftIO)
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Util.String
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.App.App ()
import Wizard.Model.App.App
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

entityName = "app"

pageLabel = "apps"

findApps :: AppContextM [App]
findApps = createFindEntitiesFn entityName

findAppsPage :: Maybe String -> Maybe Bool -> Pageable -> [Sort] -> AppContextM (Page App)
findAppsPage mQuery mEnabled pageable sort = do
  let enabledCondition =
        case mEnabled of
          Nothing -> ""
          Just True -> " AND enabled = true"
          Just False -> " AND enabled = false"
  let condition = f' "WHERE (name ~* ? OR app_id ~* ?) %s" [enabledCondition]
  createFindEntitiesPageableQuerySortFn entityName pageLabel pageable sort "*" condition [regex mQuery, regex mQuery]

findAppByUuid :: U.UUID -> AppContextM App
findAppByUuid uuid = createFindEntityByFn entityName [("uuid", U.toString uuid)]

findAppByServerDomain :: String -> AppContextM App
findAppByServerDomain serverDomain = createFindEntityByFn entityName [("server_domain", serverDomain)]

findAppByClientUrl :: String -> AppContextM App
findAppByClientUrl clientUrl = createFindEntityByFn entityName [("client_url", clientUrl)]

findAppByAppId :: String -> AppContextM App
findAppByAppId appId = createFindEntityByFn entityName [("app_id", appId)]

insertApp :: App -> AppContextM Int64
insertApp = createInsertFn entityName

updateAppByUuid :: App -> AppContextM App
updateAppByUuid app = do
  now <- liftIO getCurrentTime
  let updatedApp = app {updatedAt = now}
  let sql =
        fromString
          "UPDATE app SET uuid = ?, app_id = ?, name = ?, server_domain = ?, client_url = ?, enabled = ?, created_at = ?, updated_at = ?, server_url = ?, admin_server_url = ?, admin_client_url = ? WHERE uuid = ?"
  let params = toRow app ++ [toField updatedApp.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  return updatedApp

deleteApps :: AppContextM Int64
deleteApps = createDeleteEntitiesFn entityName

deleteAppByUuid :: U.UUID -> AppContextM Int64
deleteAppByUuid uuid = createDeleteEntityByFn entityName [("uuid", U.toString uuid)]
