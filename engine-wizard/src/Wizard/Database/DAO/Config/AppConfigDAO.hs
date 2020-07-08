module Wizard.Database.DAO.Config.AppConfigDAO where

import Data.Bson

import Shared.Database.DAO.Common
import Wizard.Database.BSON.Config.AppConfig ()
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

entityName = "appConfig"

collection = "appConfigs"

findAppConfig :: AppContextM AppConfig
findAppConfig = createFindEntityFn collection entityName

insertAppConfig :: AppConfig -> AppContextM Value
insertAppConfig = createInsertFn collection

updateAppConfig :: AppConfig -> AppContextM ()
updateAppConfig = createUpdateFn collection

deleteAppConfigs :: AppContextM ()
deleteAppConfigs = createDeleteEntitiesFn collection
