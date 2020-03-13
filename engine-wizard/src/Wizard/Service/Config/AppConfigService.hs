module Wizard.Service.Config.AppConfigService where

import Control.Monad.Reader (liftIO)
import Data.Time

import Wizard.Api.Resource.Config.AppConfigChangeDTO
import Wizard.Api.Resource.Config.AppConfigDTO
import Wizard.Database.DAO.Config.AppConfigDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.Config.AppConfigMapper
import Wizard.Service.Server.ServerService

getAppConfig :: AppContextM AppConfigDTO
getAppConfig = do
  appConfig <- findAppConfig
  return . toDTO $ appConfig

modifyAppConfig :: AppConfigChangeDTO -> AppContextM AppConfigDTO
modifyAppConfig reqDto = do
  appConfig <- findAppConfig
  now <- liftIO getCurrentTime
  let updatedAppConfig = fromDTO reqDto appConfig now
  updateAppConfig updatedAppConfig
  restartServer
  return . toDTO $ updatedAppConfig
