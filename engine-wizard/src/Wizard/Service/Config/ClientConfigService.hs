module Wizard.Service.Config.ClientConfigService where

import Control.Monad.Reader (asks)

import Shared.Model.Error.Error
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Model.Context.AppContext
import Wizard.Service.Config.ClientConfigMapper

getClientConfig :: AppContextM (Either AppError ClientConfigDTO)
getClientConfig = do
  appConfig <- asks _appContextApplicationConfig
  return . Right $ toClientConfigDTO appConfig

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetClientConfig callback = do
  eitherClientConfig <- getClientConfig
  case eitherClientConfig of
    Right clientConfig -> callback clientConfig
    Left error -> return . Left $ error
