module Service.Config.ClientConfigService where

import Control.Monad.Reader (asks)

import Api.Resource.Config.ClientConfigDTO
import Model.Context.AppContext
import Model.Error.Error
import Service.Config.ClientConfigMapper

getClientConfig :: AppContextM (Either AppError ClientConfigDTO)
getClientConfig = do
  appConfig <- asks _appContextAppConfig
  return . Right $ toDTO appConfig

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetClientConfig callback = do
  eitherClientConfig <- getClientConfig
  case eitherClientConfig of
    Right clientConfig -> callback clientConfig
    Left error -> return . Left $ error
