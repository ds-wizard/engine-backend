module Wizard.Database.DAO.App.AppDAO where

import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.App.App ()
import Wizard.Model.App.App
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

entityName = "app"

findApps :: AppContextM [App]
findApps = createFindEntitiesFn entityName

findAppById :: String -> AppContextM App
findAppById uuid = createFindEntityByFn entityName [("uuid", uuid)]

findAppByServerDomain :: String -> AppContextM App
findAppByServerDomain serverDomain = createFindEntityByFn entityName [("server_domain", serverDomain)]

findAppByClientUrl :: String -> AppContextM App
findAppByClientUrl clientUrl = createFindEntityByFn entityName [("client_url", clientUrl)]

insertApp :: App -> AppContextM Int64
insertApp = createInsertFn entityName

deleteApps :: AppContextM Int64
deleteApps = createDeleteEntitiesFn entityName
