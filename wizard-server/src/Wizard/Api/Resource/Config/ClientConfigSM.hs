module Wizard.Api.Resource.Config.ClientConfigSM where

import qualified Data.Map.Strict as M
import Data.Swagger

import Shared.Common.Api.Resource.Common.AesonSM ()
import qualified Shared.Common.Model.Config.ServerConfigDM as S_S
import Shared.Common.Util.Swagger
import Shared.OpenId.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleSM ()
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Api.Resource.Config.ClientConfigJM ()
import Wizard.Api.Resource.Plugin.PluginListSM ()
import Wizard.Api.Resource.Tenant.Config.TenantConfigSM ()
import Wizard.Api.Resource.User.UserProfileSM ()
import Wizard.Database.Migration.Development.Plugin.Data.Plugins
import qualified Wizard.Database.Migration.Development.Tenant.Data.TenantConfigs as TC
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Config.ServerConfig ()
import qualified Wizard.Model.Config.ServerConfigDM as S
import Wizard.Service.Config.Client.ClientConfigMapper
import Wizard.Service.User.UserMapper
import WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientSimpleSM ()
import WizardLib.Public.Database.Migration.Development.OpenId.Data.OpenIdClients
import qualified WizardLib.Public.Database.Migration.Development.Tenant.Data.TenantConfigs as STC

instance ToSchema ClientConfigDTO where
  declareNamedSchema = toSwaggerWithType "type" (toClientConfigDTO S.defaultConfig TC.defaultOrganization TC.defaultAuthentication [defaultOpenIdClientSimple] TC.defaultPrivacyAndSupport TC.defaultDashboardAndLoginScreen STC.defaultLookAndFeel TC.defaultRegistry TC.defaultProject TC.defaultSubmission STC.defaultFeatures TC.defaultOwl (Just $ toUserProfile (toDTO userAlbert) [] M.empty) [] [plugin1List] M.empty defaultTenantModules defaultTenant)

instance ToSchema ClientConfigAuthDTO where
  declareNamedSchema = toSwagger (toClientAuthDTO TC.defaultAuthentication [defaultOpenIdClientSimple])

instance ToSchema ClientConfigAuthExternalDTO where
  declareNamedSchema = toSwagger (toClientAuthExternalDTO [defaultOpenIdClientSimple])

instance ToSchema ClientConfigRegistryDTO where
  declareNamedSchema = toSwagger (toClientConfigRegistryDTO S.defaultRegistry TC.defaultRegistry)

instance ToSchema ClientConfigProjectDTO where
  declareNamedSchema = toSwagger (toClientConfigProjectDTO TC.defaultProject)

instance ToSchema ClientConfigCloudDTO where
  declareNamedSchema = toSwagger (toClientConfigCloudDTO S_S.defaultCloud defaultTenant)

instance ToSchema ClientConfigAdminDTO where
  declareNamedSchema = toSwagger (toClientConfigAdminDTO S.defaultAdmin)

instance ToSchema ClientConfigFeaturesDTO where
  declareNamedSchema = toSwagger (toClientConfigFeaturesDTO S.defaultAdmin STC.defaultFeatures)

instance ToSchema ClientConfigSignalBridgeDTO where
  declareNamedSchema = toSwagger (toClientConfigSignalBridgeDTO defaultTenant)

instance ToSchema ClientConfigModuleDTO where
  declareNamedSchema = toSwagger (toModuleDTO defaultTenantModule)
