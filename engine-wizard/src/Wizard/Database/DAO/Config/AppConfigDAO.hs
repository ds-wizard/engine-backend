module Wizard.Database.DAO.Config.AppConfigDAO where

import Data.Bson

import Wizard.Database.BSON.Config.AppConfig ()
import Wizard.Database.DAO.Common
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext

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
