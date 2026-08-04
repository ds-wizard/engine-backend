module Wizard.Service.Config.Client.ClientConfigMapper where

import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Config.SimpleFeature
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Plugin.PluginList
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.Tenant.Tenant
import Wizard.Model.User.UserProfile
import WizardLib.Public.Model.OpenId.OpenIdClientSimple
import WizardLib.Public.Model.Tenant.Config.TenantConfig
import WizardLib.Public.Model.Tenant.Module.TenantModule

toClientConfigDTO :: ServerConfig -> TenantConfigOrganization -> TenantConfigAuthentication -> [OpenIdClientSimple] -> TenantConfigPrivacyAndSupport -> TenantConfigDashboardAndLoginScreen -> TenantConfigLookAndFeel -> TenantConfigRegistry -> TenantConfigProject -> TenantConfigSubmission -> TenantConfigFeatures -> TenantConfigOwl -> Maybe UserProfile -> [String] -> [PluginList] -> M.Map U.UUID A.Value -> [TenantModule] -> Tenant -> ClientConfigDTO
toClientConfigDTO serverConfig tcOrganization tcAuthentication openIdClients tcPrivacyAndSupport tcDashboardAndLoginScreen tcLookAndFeel tcRegistry tcProject tcSubmission tcFeatures tcOwl mUserProfile tours plugins pluginSettings tenantModules tenant =
  ClientConfigDTO
    { user = mUserProfile
    , tours = tours
    , organization = tcOrganization
    , authentication = toClientAuthDTO tcAuthentication openIdClients
    , privacyAndSupport = tcPrivacyAndSupport
    , dashboardAndLoginScreen = tcDashboardAndLoginScreen
    , lookAndFeel = tcLookAndFeel
    , registry = toClientConfigRegistryDTO serverConfig.registry tcRegistry
    , project = toClientConfigProjectDTO tcProject
    , submission = SimpleFeature tcSubmission.enabled
    , cloud = toClientConfigCloudDTO serverConfig.cloud tenant
    , owl = tcOwl
    , admin = toClientConfigAdminDTO serverConfig.admin
    , features = toClientConfigFeaturesDTO serverConfig.admin tcFeatures
    , plugins = plugins
    , pluginSettings = pluginSettings
    , signalBridge = toClientConfigSignalBridgeDTO tenant
    , modules =
        if serverConfig.admin.enabled
          then case mUserProfile of
            Just userProfile ->
              let perms = userProfile.role.permissions
               in [toModuleDTO m | m <- tenantModules, m.enabled, maybe True (`elem` perms) m.requiredPermission]
            Nothing -> []
          else []
    }

toClientAuthDTO :: TenantConfigAuthentication -> [OpenIdClientSimple] -> ClientConfigAuthDTO
toClientAuthDTO tcAuthentication openIdClients =
  ClientConfigAuthDTO
    { defaultRoleUuid = tcAuthentication.defaultRoleUuid
    , internal = tcAuthentication.internal
    , external = toClientAuthExternalDTO openIdClients
    }

toClientAuthExternalDTO :: [OpenIdClientSimple] -> ClientConfigAuthExternalDTO
toClientAuthExternalDTO openIdClients =
  ClientConfigAuthExternalDTO
    { services = openIdClients
    }

toClientConfigRegistryDTO :: ServerConfigRegistry -> TenantConfigRegistry -> ClientConfigRegistryDTO
toClientConfigRegistryDTO serverConfig tenantConfig =
  ClientConfigRegistryDTO
    { enabled = tenantConfig.enabled
    , url = serverConfig.clientUrl
    }

toClientConfigProjectDTO :: TenantConfigProject -> ClientConfigProjectDTO
toClientConfigProjectDTO tenantConfig =
  ClientConfigProjectDTO
    { projectVisibility = tenantConfig.projectVisibility
    , projectSharing = tenantConfig.projectSharing
    , projectCreation = tenantConfig.projectCreation
    , projectTagging = SimpleFeature tenantConfig.projectTagging.enabled
    , summaryReport = tenantConfig.summaryReport
    , feedback = SimpleFeature tenantConfig.feedback.enabled
    }

toClientConfigCloudDTO :: ServerConfigCloud -> Tenant -> ClientConfigCloudDTO
toClientConfigCloudDTO serverConfig tenant =
  ClientConfigCloudDTO
    { enabled = serverConfig.enabled
    , serverUrl = tenant.serverUrl
    }

toClientConfigAdminDTO :: ServerConfigAdmin -> ClientConfigAdminDTO
toClientConfigAdminDTO serverConfig =
  ClientConfigAdminDTO {enabled = serverConfig.enabled}

toClientConfigFeaturesDTO :: ServerConfigAdmin -> TenantConfigFeatures -> ClientConfigFeaturesDTO
toClientConfigFeaturesDTO serverConfig tenantConfig =
  ClientConfigFeaturesDTO
    { aiAssistantEnabled = serverConfig.enabled && tenantConfig.aiAssistantEnabled
    , toursEnabled = tenantConfig.toursEnabled
    }

toClientConfigSignalBridgeDTO :: Tenant -> ClientConfigSignalBridgeDTO
toClientConfigSignalBridgeDTO tenant =
  ClientConfigSignalBridgeDTO {webSocketUrl = tenant.signalBridgeUrl}

toModuleDTO :: TenantModule -> ClientConfigModuleDTO
toModuleDTO tenantModule =
  ClientConfigModuleDTO
    { title = tenantModule.title
    , description = tenantModule.description
    , icon = tenantModule.icon
    , url = tenantModule.url
    , external = tenantModule.external
    }
