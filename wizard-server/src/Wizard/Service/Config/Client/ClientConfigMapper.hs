module Wizard.Service.Config.Client.ClientConfigMapper where

import Data.Maybe

import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Config.SimpleFeature
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.Tenant.Tenant
import Wizard.Model.User.UserProfile

toClientConfigDTO :: ServerConfig -> TenantConfig -> Maybe UserProfile -> Tenant -> ClientConfigDTO
toClientConfigDTO serverConfig tenantConfig mUserProfile tenant =
  ClientConfigDTO
    { user = mUserProfile
    , organization = tenantConfig.organization
    , authentication = toClientAuthDTO $ tenantConfig.authentication
    , privacyAndSupport = tenantConfig.privacyAndSupport
    , dashboardAndLoginScreen = tenantConfig.dashboardAndLoginScreen
    , lookAndFeel = tenantConfig.lookAndFeel
    , registry = toClientConfigRegistryDTO serverConfig.registry tenantConfig.registry
    , questionnaire = toClientConfigQuestionnaireDTO $ tenantConfig.questionnaire
    , submission = SimpleFeature $ tenantConfig.submission.enabled
    , cloud = toClientConfigCloudDTO serverConfig.cloud tenant
    , owl = tenantConfig.owl
    , admin = toClientConfigAdminDTO serverConfig.admin tenant
    , aiAssistant = toClientConfigAiAssistantDTO serverConfig.admin tenantConfig.aiAssistant
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
            Nothing -> []
          else []
    }

toClientAuthDTO :: TenantConfigAuth -> ClientConfigAuthDTO
toClientAuthDTO config =
  ClientConfigAuthDTO
    { defaultRole = config.defaultRole
    , internal = config.internal
    , external = toClientAuthExternalDTO $ config.external
    }

toClientAuthExternalDTO :: TenantConfigAuthExternal -> ClientConfigAuthExternalDTO
toClientAuthExternalDTO config =
  ClientConfigAuthExternalDTO
    { services = toClientAuthExternalServiceDTO <$> config.services
    }

toClientAuthExternalServiceDTO :: TenantConfigAuthExternalService -> ClientConfigAuthExternalServiceDTO
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
