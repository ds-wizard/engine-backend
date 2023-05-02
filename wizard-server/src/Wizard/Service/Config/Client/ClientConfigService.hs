module Wizard.Service.Config.Client.ClientConfigService where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)

import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Database.DAO.App.AppDAO
import Wizard.Database.DAO.Locale.LocaleDAO
import Wizard.Model.App.App
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Service.App.AppHelper
import Wizard.Service.Config.App.AppConfigService
import Wizard.Service.Config.Client.ClientConfigMapper

getClientConfig :: Maybe String -> AppContextM ClientConfigDTO
getClientConfig mClientUrl = do
  serverConfig <- asks serverConfig
  app <-
    if serverConfig.cloud.enabled
      then maybe getCurrentApp findAppByClientUrl mClientUrl
      else getCurrentApp
  unless app.enabled (throwError LockedError)
  appConfig <-
    if serverConfig.cloud.enabled
      then case mClientUrl of
        Just clientUrl -> getAppConfigByUuid app.uuid
        Nothing -> getAppConfig
      else getAppConfig
  locales <- findLocalesFilteredWithApp app.uuid [("enabled", show True)]
  return $ toClientConfigDTO serverConfig appConfig app locales
