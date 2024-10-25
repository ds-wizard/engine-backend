module Wizard.Api.Resource.Config.ClientConfigSM where

import Data.Swagger

import qualified Shared.Common.Model.Config.ServerConfigDM as S_S
import Shared.Common.Util.Swagger
import Shared.Locale.Database.Migration.Development.Locale.Data.Locales
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Api.Resource.Config.ClientConfigJM ()
import Wizard.Api.Resource.Locale.LocaleSM ()
import Wizard.Api.Resource.Tenant.Config.TenantConfigSM ()
import Wizard.Api.Resource.User.UserProfileSM ()
import qualified Wizard.Database.Migration.Development.Tenant.Data.TenantConfigs as A
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Config.ServerConfig
import qualified Wizard.Model.Config.ServerConfigDM as S
import Wizard.Service.Config.Client.ClientConfigMapper
import Wizard.Service.User.UserMapper

instance ToSchema ClientConfigDTO where
  declareNamedSchema = toSwaggerWithType "type" (toClientConfigDTO S.defaultConfig A.defaultTenantConfig (Just $ toUserProfile (toDTO userAlbert) []) defaultTenant [])

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
  declareNamedSchema = toSwagger (toClientConfigCloudDTO S_S.defaultCloud defaultTenant)

instance ToSchema ClientConfigLocaleDTO where
  declareNamedSchema = toSwagger (toClientConfigLocaleDTO localeNl)

instance ToSchema ClientConfigAdminDTO where
  declareNamedSchema = toSwagger (toClientConfigAdminDTO S.defaultAdmin defaultTenant)

instance ToSchema ClientConfigAiAssistantDTO where
  declareNamedSchema = toSwagger (toClientConfigAiAssistantDTO S.defaultAdmin defaultTenant)

instance ToSchema ClientConfigSignalBridgeDTO where
  declareNamedSchema = toSwagger (toClientConfigSignalBridgeDTO defaultTenant)

instance ToSchema ClientConfigModuleDTO where
  declareNamedSchema = toSwagger (toClientConfigModuleDTO S.defaultConfig.modules.wizard "https://wizard-client.com" False)
