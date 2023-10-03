module Wizard.Service.Config.Client.ClientConfigMapper where

import Data.Maybe

import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Config.SimpleFeature
import Shared.Locale.Model.Locale.Locale
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Model.App.App
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig

toClientConfigDTO :: ServerConfig -> AppConfig -> App -> [Locale] -> ClientConfigDTO
toClientConfigDTO serverConfig appConfig app locales =
  ClientConfigDTO
    { organization = appConfig.organization
    , feature = appConfig.feature
    , authentication = toClientAuthDTO $ appConfig.authentication
    , privacyAndSupport = appConfig.privacyAndSupport
    , dashboardAndLoginScreen = appConfig.dashboardAndLoginScreen
    , lookAndFeel = appConfig.lookAndFeel
    , registry = toClientConfigRegistryDTO serverConfig.registry appConfig.registry
    , questionnaire = toClientConfigQuestionnaireDTO $ appConfig.questionnaire
    , submission = SimpleFeature $ appConfig.submission.enabled
    , cloud = toClientConfigCloudDTO serverConfig.cloud app
    , locales = fmap toClientConfigLocaleDTO locales
    , owl = appConfig.owl
    , admin = toClientConfigAdminDTO serverConfig.admin app
    , modules =
        if serverConfig.admin.enabled
          then
            [ toClientConfigModuleDTO serverConfig.modules.wizard app.clientUrl False
            , toClientConfigModuleDTO serverConfig.modules.admin (fromMaybe "" app.adminClientUrl) False
            , toClientConfigModuleDTO serverConfig.modules.guide (fromMaybe "" serverConfig.modules.guide.url) True
            ]
          else []
    }

toClientAuthDTO :: AppConfigAuth -> ClientConfigAuthDTO
toClientAuthDTO config =
  ClientConfigAuthDTO
    { defaultRole = config.defaultRole
    , internal = config.internal
    , external = toClientAuthExternalDTO $ config.external
    }

toClientAuthExternalDTO :: AppConfigAuthExternal -> ClientConfigAuthExternalDTO
toClientAuthExternalDTO config =
  ClientConfigAuthExternalDTO
    { services = toClientAuthExternalServiceDTO <$> config.services
    }

toClientAuthExternalServiceDTO :: AppConfigAuthExternalService -> ClientConfigAuthExternalServiceDTO
toClientAuthExternalServiceDTO config =
  ClientConfigAuthExternalServiceDTO
    { aId = config.aId
    , name = config.name
    , url = config.url
    , style = config.style
    }

toClientConfigRegistryDTO :: ServerConfigRegistry -> AppConfigRegistry -> ClientConfigRegistryDTO
toClientConfigRegistryDTO serverConfig appConfig =
  ClientConfigRegistryDTO
    { enabled = appConfig.enabled
    , url = serverConfig.clientUrl
    }

toClientConfigQuestionnaireDTO :: AppConfigQuestionnaire -> ClientConfigQuestionnaireDTO
toClientConfigQuestionnaireDTO appConfig =
  ClientConfigQuestionnaireDTO
    { questionnaireVisibility = appConfig.questionnaireVisibility
    , questionnaireSharing = appConfig.questionnaireSharing
    , questionnaireCreation = appConfig.questionnaireCreation
    , projectTagging = SimpleFeature $ appConfig.projectTagging.enabled
    , summaryReport = appConfig.summaryReport
    , feedback = SimpleFeature $ appConfig.feedback.enabled
    }

toClientConfigCloudDTO :: ServerConfigCloud -> App -> ClientConfigCloudDTO
toClientConfigCloudDTO serverConfig app =
  ClientConfigCloudDTO
    { enabled = serverConfig.enabled
    , serverUrl = app.serverUrl
    }

toClientConfigLocaleDTO :: Locale -> ClientConfigLocaleDTO
toClientConfigLocaleDTO locale =
  ClientConfigLocaleDTO {name = locale.name, code = locale.code, defaultLocale = locale.defaultLocale}

toClientConfigAdminDTO :: ServerConfigAdmin -> App -> ClientConfigAdminDTO
toClientConfigAdminDTO serverConfig app =
  ClientConfigAdminDTO {enabled = serverConfig.enabled, clientUrl = app.adminClientUrl}

toClientConfigModuleDTO :: ServerConfigModule -> String -> Bool -> ClientConfigModuleDTO
toClientConfigModuleDTO serverConfig url external =
  ClientConfigModuleDTO
    { title = serverConfig.title
    , description = serverConfig.description
    , icon = serverConfig.icon
    , url = url
    , external = external
    }
