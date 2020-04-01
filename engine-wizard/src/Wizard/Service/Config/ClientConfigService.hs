module Wizard.Service.Config.ClientConfigService where

import Control.Monad.Reader (asks)

import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Model.Context.AppContext
import Wizard.Service.Config.AppConfigService
import Wizard.Service.Config.ClientConfigMapper

getClientConfig :: AppContextM ClientConfigDTO
getClientConfig = do
  appConfig <- getAppConfig
  serverConfig <- asks _appContextServerConfig
  return $ toClientConfigDTO serverConfig appConfig
