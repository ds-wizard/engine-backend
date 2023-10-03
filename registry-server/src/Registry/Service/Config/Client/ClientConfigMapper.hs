module Registry.Service.Config.Client.ClientConfigMapper where

import Registry.Api.Resource.Config.ClientConfigDTO
import Registry.Model.Config.ServerConfig

toClientConfigDTO :: ServerConfig -> ClientConfigDTO
toClientConfigDTO serverConfig =
  ClientConfigDTO
    { authentication = toClientAuthDTO serverConfig
    }

toClientAuthDTO :: ServerConfig -> ClientConfigAuthDTO
toClientAuthDTO serverConfig =
  ClientConfigAuthDTO
    { publicRegistrationEnabled = serverConfig.general.publicRegistrationEnabled
    }
