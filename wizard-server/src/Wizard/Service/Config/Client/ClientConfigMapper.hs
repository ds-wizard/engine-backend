module Wizard.Service.Config.Client.ClientConfigMapper where

import Data.Maybe

import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Config.SimpleFeature
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.Tenant.Tenant
import Wizard.Model.User.UserProfile
import WizardLib.Public.Model.Tenant.Config.TenantConfig

toClientConfigDTO :: ServerConfig -> TenantConfigOrganization -> TenantConfigAuthentication -> TenantConfigPrivacyAndSupport -> TenantConfigDashboardAndLoginScreen -> TenantConfigLookAndFeel -> TenantConfigRegistry -> TenantConfigQuestionnaire -> TenantConfigSubmission -> TenantConfigAiAssistant -> TenantConfigOwl -> Maybe UserProfile -> [String] -> Tenant -> ClientConfigDTO
toClientConfigDTO serverConfig tcOrganization tcAuthentication tcPrivacyAndSupport tcDashboardAndLoginScreen tcLookAndFeel tcRegistry tcQuestionnaire tcSubmission tcAiAssistant tcOwl mUserProfile tours tenant =
  ClientConfigDTO
    { user = mUserProfile
    , tours = tours
    , organization = tcOrganization
    , authentication = toClientAuthDTO tcAuthentication
    , privacyAndSupport = tcPrivacyAndSupport
    , dashboardAndLoginScreen = tcDashboardAndLoginScreen
    , lookAndFeel = tcLookAndFeel
    , registry = toClientConfigRegistryDTO serverConfig.registry tcRegistry
    , questionnaire = toClientConfigQuestionnaireDTO tcQuestionnaire
    , submission = SimpleFeature $ tcSubmission.enabled
    , cloud = toClientConfigCloudDTO serverConfig.cloud tenant
    , owl = tcOwl
    , admin = toClientConfigAdminDTO serverConfig.admin tenant
    , aiAssistant = toClientConfigAiAssistantDTO serverConfig.admin tcAiAssistant
    , signalBridge = toClientConfigSignalBridgeDTO tenant
    , modules =
        if serverConfig.admin.enabled
          then case mUserProfile of
            Just userProfile ->
              case userProfile.uRole of
                "admin" ->
                  [ toClientConfigModuleDTO serverConfig.modules.wizard tenant.clientUrl False
                  , toClientConfigModuleDTO serverConfig.modules.admin (fromMaybe "" tenant.adminClientUrl) False
                  , toClientConfigModuleDTO serverConfig.modules.integrationHub (fromMaybe "" tenant.integrationHubClientUrl) False
                  , toClientConfigModuleDTO serverConfig.modules.analytics (fromMaybe "" tenant.analyticsClientUrl) False
                  , toClientConfigModuleDTO serverConfig.modules.guide (fromMaybe "" serverConfig.modules.guide.url) True
                  ]
                "dataSteward" ->
                  [ toClientConfigModuleDTO serverConfig.modules.wizard tenant.clientUrl False
                  , toClientConfigModuleDTO serverConfig.modules.admin (fromMaybe "" tenant.adminClientUrl) False
                  , toClientConfigModuleDTO serverConfig.modules.integrationHub (fromMaybe "" tenant.integrationHubClientUrl) False
                  , toClientConfigModuleDTO serverConfig.modules.guide (fromMaybe "" serverConfig.modules.guide.url) True
                  ]
                "researcher" ->
                  [ toClientConfigModuleDTO serverConfig.modules.wizard tenant.clientUrl False
                  , toClientConfigModuleDTO serverConfig.modules.admin (fromMaybe "" tenant.adminClientUrl) False
                  , toClientConfigModuleDTO serverConfig.modules.guide (fromMaybe "" serverConfig.modules.guide.url) True
                  ]
                _ -> []
            Nothing -> []
          else []
    }

toClientAuthDTO :: TenantConfigAuthentication -> ClientConfigAuthDTO
toClientAuthDTO tcAuthentication =
  ClientConfigAuthDTO
    { defaultRole = tcAuthentication.defaultRole
    , internal = tcAuthentication.internal
    , external = toClientAuthExternalDTO tcAuthentication.external
    }

toClientAuthExternalDTO :: TenantConfigAuthenticationExternal -> ClientConfigAuthExternalDTO
toClientAuthExternalDTO config =
  ClientConfigAuthExternalDTO
    { services = toClientAuthExternalServiceDTO <$> config.services
    }

toClientAuthExternalServiceDTO :: TenantConfigAuthenticationExternalService -> ClientConfigAuthExternalServiceDTO
toClientAuthExternalServiceDTO config =
  ClientConfigAuthExternalServiceDTO
    { aId = config.aId
    , name = config.name
    , url = config.url
    , style = config.style
    }

toClientConfigRegistryDTO :: ServerConfigRegistry -> TenantConfigRegistry -> ClientConfigRegistryDTO
toClientConfigRegistryDTO serverConfig tenantConfig =
  ClientConfigRegistryDTO
    { enabled = tenantConfig.enabled
    , url = serverConfig.clientUrl
    }

toClientConfigQuestionnaireDTO :: TenantConfigQuestionnaire -> ClientConfigQuestionnaireDTO
toClientConfigQuestionnaireDTO tenantConfig =
  ClientConfigQuestionnaireDTO
    { questionnaireVisibility = tenantConfig.questionnaireVisibility
    , questionnaireSharing = tenantConfig.questionnaireSharing
    , questionnaireCreation = tenantConfig.questionnaireCreation
    , projectTagging = SimpleFeature $ tenantConfig.projectTagging.enabled
    , summaryReport = tenantConfig.summaryReport
    , feedback = SimpleFeature $ tenantConfig.feedback.enabled
    }

toClientConfigCloudDTO :: ServerConfigCloud -> Tenant -> ClientConfigCloudDTO
toClientConfigCloudDTO serverConfig tenant =
  ClientConfigCloudDTO
    { enabled = serverConfig.enabled
    , serverUrl = tenant.serverUrl
    }

toClientConfigAdminDTO :: ServerConfigAdmin -> Tenant -> ClientConfigAdminDTO
toClientConfigAdminDTO serverConfig tenant =
  ClientConfigAdminDTO {enabled = serverConfig.enabled, clientUrl = tenant.adminClientUrl}

toClientConfigAiAssistantDTO :: ServerConfigAdmin -> TenantConfigAiAssistant -> ClientConfigAiAssistantDTO
toClientConfigAiAssistantDTO serverConfig tenantConfig =
  ClientConfigAiAssistantDTO
    { enabled = serverConfig.enabled && tenantConfig.enabled
    }

toClientConfigSignalBridgeDTO :: Tenant -> ClientConfigSignalBridgeDTO
toClientConfigSignalBridgeDTO tenant =
  ClientConfigSignalBridgeDTO {webSocketUrl = tenant.signalBridgeUrl}

toClientConfigModuleDTO :: ServerConfigModule -> String -> Bool -> ClientConfigModuleDTO
toClientConfigModuleDTO serverConfig url external =
  ClientConfigModuleDTO
    { title = serverConfig.title
    , description = serverConfig.description
    , icon = serverConfig.icon
    , url = url
    , external = external
    }
