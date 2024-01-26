module Wizard.Service.Config.Client.ClientConfigMapper where

import Data.Maybe

import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Config.SimpleFeature
import Shared.Locale.Model.Locale.Locale
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.Tenant.Tenant
import Wizard.Model.User.User
import Wizard.Model.User.UserProfile

toClientConfigDTO :: ServerConfig -> TenantConfig -> Maybe UserProfile -> Tenant -> [Locale] -> ClientConfigDTO
toClientConfigDTO serverConfig tenantConfig mUserProfile tenant locales =
  ClientConfigDTO
    { user = mUserProfile
    , organization = tenantConfig.organization
    , feature = tenantConfig.feature
    , authentication = toClientAuthDTO $ tenantConfig.authentication
    , privacyAndSupport = tenantConfig.privacyAndSupport
    , dashboardAndLoginScreen = tenantConfig.dashboardAndLoginScreen
    , lookAndFeel = tenantConfig.lookAndFeel
    , registry = toClientConfigRegistryDTO serverConfig.registry tenantConfig.registry
    , questionnaire = toClientConfigQuestionnaireDTO $ tenantConfig.questionnaire
    , submission = SimpleFeature $ tenantConfig.submission.enabled
    , cloud = toClientConfigCloudDTO serverConfig.cloud tenant
    , locales = fmap toClientConfigLocaleDTO locales
    , owl = tenantConfig.owl
    , admin = toClientConfigAdminDTO serverConfig.admin tenant
    , modules =
        if serverConfig.admin.enabled
          then case mUserProfile of
            Just userProfile ->
              if userProfile.uRole == _USER_ROLE_ADMIN || userProfile.uRole == _USER_ROLE_DATA_STEWARD
                then
                  [ toClientConfigModuleDTO serverConfig.modules.wizard tenant.clientUrl False
                  , toClientConfigModuleDTO serverConfig.modules.admin (fromMaybe "" tenant.adminClientUrl) False
                  , toClientConfigModuleDTO serverConfig.modules.integrationHub (fromMaybe "" tenant.integrationHubClientUrl) False
                  , toClientConfigModuleDTO serverConfig.modules.reporting (fromMaybe "" tenant.reportingClientUrl) False
                  , toClientConfigModuleDTO serverConfig.modules.guide (fromMaybe "" serverConfig.modules.guide.url) True
                  ]
                else
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

toClientConfigLocaleDTO :: Locale -> ClientConfigLocaleDTO
toClientConfigLocaleDTO locale =
  ClientConfigLocaleDTO {name = locale.name, code = locale.code, defaultLocale = locale.defaultLocale}

toClientConfigAdminDTO :: ServerConfigAdmin -> Tenant -> ClientConfigAdminDTO
toClientConfigAdminDTO serverConfig tenant =
  ClientConfigAdminDTO {enabled = serverConfig.enabled, clientUrl = tenant.adminClientUrl}

toClientConfigModuleDTO :: ServerConfigModule -> String -> Bool -> ClientConfigModuleDTO
toClientConfigModuleDTO serverConfig url external =
  ClientConfigModuleDTO
    { title = serverConfig.title
    , description = serverConfig.description
    , icon = serverConfig.icon
    , url = url
    , external = external
    }
