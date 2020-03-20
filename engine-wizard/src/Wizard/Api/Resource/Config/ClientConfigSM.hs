module Wizard.Api.Resource.Config.ClientConfigSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Config.AppConfigSM ()
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Api.Resource.Config.ClientConfigJM ()
import Wizard.Database.Migration.Development.Config.Data.AppConfigs
import Wizard.Model.Config.ServerConfigDM
import Wizard.Service.Config.ClientConfigMapper

instance ToSchema ClientConfigDTO where
  declareNamedSchema = simpleToSchema "_clientConfigDTO" (toClientConfigDTO defaultConfig defaultAppConfig)

instance ToSchema ClientConfigFeaturesDTO where
  declareNamedSchema =
    simpleToSchema "_clientConfigFeaturesDTO" (toClientConfigFeaturesDTO defaultConfig defaultAppConfig)

instance ToSchema ClientConfigRegistryDTO where
  declareNamedSchema = simpleToSchema "_clientConfigRegistryDTO" (toClientConfigRegistryDTO defaultRegistry)
