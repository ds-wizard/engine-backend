module Wizard.Api.Resource.Config.ClientConfigSM where

import Data.Swagger

import qualified Shared.Common.Model.Config.ServerConfigDM as S_S
import Shared.Common.Util.Swagger
import Shared.Locale.Database.Migration.Development.Locale.Data.Locales
import Wizard.Api.Resource.Config.AppConfigSM ()
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Api.Resource.Config.ClientConfigJM ()
import Wizard.Api.Resource.Locale.LocaleSM ()
import Wizard.Database.Migration.Development.App.Data.Apps
import qualified Wizard.Database.Migration.Development.Config.Data.AppConfigs as A
import qualified Wizard.Model.Config.ServerConfigDM as S
import Wizard.Service.Config.Client.ClientConfigMapper

instance ToSchema ClientConfigDTO where
  declareNamedSchema = toSwagger (toClientConfigDTO S.defaultConfig A.defaultAppConfig defaultApp [])

instance ToSchema ClientConfigAuthDTO where
  declareNamedSchema = toSwagger (toClientAuthDTO A.defaultAuth)

instance ToSchema ClientConfigAuthExternalDTO where
  declareNamedSchema = toSwagger (toClientAuthExternalDTO A.defaultAuthExternal)

instance ToSchema ClientConfigAuthExternalServiceDTO where
  declareNamedSchema = toSwagger (toClientAuthExternalServiceDTO A.defaultAuthExternalService)

instance ToSchema ClientConfigRegistryDTO where
  declareNamedSchema = toSwagger (toClientConfigRegistryDTO S.defaultRegistry A.defaultRegistry)

instance ToSchema ClientConfigQuestionnaireDTO where
  declareNamedSchema = toSwagger (toClientConfigQuestionnaireDTO A.defaultQuestionnaire)

instance ToSchema ClientConfigCloudDTO where
  declareNamedSchema = toSwagger (toClientConfigCloudDTO S_S.defaultCloud defaultApp)

instance ToSchema ClientConfigLocaleDTO where
  declareNamedSchema = toSwagger (toClientConfigLocaleDTO localeNl)

instance ToSchema ClientConfigAdminDTO where
  declareNamedSchema = toSwagger (toClientConfigAdminDTO S.defaultAdmin defaultApp)
