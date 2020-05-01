module Wizard.Api.Resource.Config.AppConfigChangeSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Config.AppConfigChangeDTO
import Wizard.Api.Resource.Config.AppConfigChangeJM ()
import Wizard.Api.Resource.Config.AppConfigSM ()
import Wizard.Database.Migration.Development.Config.Data.AppConfigs
import Wizard.Service.Config.AppConfigMapper

instance ToSchema AppConfigChangeDTO where
  declareNamedSchema = simpleToSchema (toChangeDTO defaultAppConfig)
