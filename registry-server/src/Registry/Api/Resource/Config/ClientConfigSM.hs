module Registry.Api.Resource.Config.ClientConfigSM where

import Data.Swagger

import Registry.Api.Resource.Config.ClientConfigDTO
import Registry.Api.Resource.Config.ClientConfigJM ()
import qualified Registry.Model.Config.ServerConfigDM as S
import Registry.Service.Config.Client.ClientConfigMapper
import Shared.Common.Util.Swagger

instance ToSchema ClientConfigDTO where
  declareNamedSchema = toSwagger (toClientConfigDTO S.defaultConfig)

instance ToSchema ClientConfigAuthDTO where
  declareNamedSchema = toSwagger (toClientAuthDTO S.defaultConfig)

instance ToSchema ClientConfigLocaleDTO where
  declareNamedSchema = toSwagger (toClientLocaleDTO S.defaultConfig)
