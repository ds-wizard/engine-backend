module Wizard.Service.Config.ClientConfigService where

import Control.Monad.Reader (asks)

import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Database.DAO.Config.AppConfigDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.Config.ClientConfigMapper

getClientConfig :: AppContextM ClientConfigDTO
getClientConfig = do
  appConfig <- findAppConfig
  serverConfig <- asks _appContextApplicationConfig
  return $ toClientConfigDTO serverConfig appConfig
