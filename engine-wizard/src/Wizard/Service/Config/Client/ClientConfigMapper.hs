module Wizard.Service.Config.Client.ClientConfigMapper where

import Shared.Model.Config.ServerConfig
import Shared.Model.Locale.Locale
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Model.App.App
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Config.SimpleFeature

toClientConfigDTO :: ServerConfig -> AppConfig -> App -> [Locale] -> ClientConfigDTO
toClientConfigDTO serverConfig appConfig app locales =
  ClientConfigDTO
    { organization = appConfig.organization
    , feature = appConfig.feature
    , authentication = toClientAuthDTO $ appConfig.authentication
    , privacyAndSupport = appConfig.privacyAndSupport
    , dashboard = appConfig.dashboard
    , lookAndFeel = appConfig.lookAndFeel
    , registry = toClientConfigRegistryDTO serverConfig.registry appConfig.registry
    , questionnaire = toClientConfigQuestionnaireDTO $ appConfig.questionnaire
    , submission = SimpleFeature $ appConfig.submission.enabled
    , cloud = toClientConfigCloudDTO serverConfig.cloud app
    , locales = fmap toClientConfigLocaleDTO locales
    , owl = appConfig.owl
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
