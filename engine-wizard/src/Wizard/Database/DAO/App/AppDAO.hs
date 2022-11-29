module Wizard.Database.DAO.App.AppDAO where

import Control.Monad.Reader (liftIO)
import Data.String
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Util.String
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

findAppById :: String -> AppContextM App
findAppById uuid = createFindEntityByFn entityName [("uuid", uuid)]

findAppByServerDomain :: String -> AppContextM App
findAppByServerDomain serverDomain = createFindEntityByFn entityName [("server_domain", serverDomain)]

findAppByClientUrl :: String -> AppContextM App
findAppByClientUrl clientUrl = createFindEntityByFn entityName [("client_url", clientUrl)]

findAppByAppId :: String -> AppContextM App
findAppByAppId appId = createFindEntityByFn entityName [("app_id", appId)]

insertApp :: App -> AppContextM Int64
insertApp = createInsertFn entityName

updateAppById :: App -> AppContextM App
updateAppById app = do
  now <- liftIO getCurrentTime
  let updatedApp = app {updatedAt = now}
  let sql =
        fromString
          "UPDATE app SET uuid = ?, app_id = ?, name = ?, server_domain = ?, client_url = ?, enabled = ?, created_at = ?, updated_at = ?, server_url = ? WHERE uuid = ?"
  let params = toRow app ++ [toField updatedApp.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  return updatedApp

deleteApps :: AppContextM Int64
deleteApps = createDeleteEntitiesFn entityName

deleteAppById :: String -> AppContextM Int64
deleteAppById uuid = createDeleteEntityByFn entityName [("uuid", uuid)]
