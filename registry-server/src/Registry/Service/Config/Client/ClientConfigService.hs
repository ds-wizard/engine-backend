module Registry.Service.Config.Client.ClientConfigService where

import Control.Monad.Reader (asks)

import Registry.Api.Resource.Config.ClientConfigDTO
import Registry.Model.Context.AppContext
import Registry.Service.Config.Client.ClientConfigMapper

getClientConfig :: AppContextM ClientConfigDTO
getClientConfig = do
  serverConfig <- asks serverConfig
  return $ toClientConfigDTO serverConfig
