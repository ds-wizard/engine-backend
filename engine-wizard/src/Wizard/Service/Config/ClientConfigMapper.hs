module Wizard.Service.Config.ClientConfigMapper where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Config.ServerConfig
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Model.App.App
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Config.SimpleFeature
import Wizard.Model.Locale.Locale
import Wizard.Service.Locale.LocaleMapper

toClientConfigDTO :: ServerConfig -> AppConfig -> App -> [Locale] -> ClientConfigDTO
toClientConfigDTO serverConfig appConfig app locales =
  ClientConfigDTO
    { _clientConfigDTOOrganization = appConfig ^. organization
    , _clientConfigDTOFeature = appConfig ^. feature
    , _clientConfigDTOAuthentication = toClientAuthDTO $ appConfig ^. authentication
    , _clientConfigDTOPrivacyAndSupport = appConfig ^. privacyAndSupport
    , _clientConfigDTODashboard = appConfig ^. dashboard
    , _clientConfigDTOLookAndFeel = appConfig ^. lookAndFeel
    , _clientConfigDTORegistry = toClientConfigRegistryDTO (serverConfig ^. registry) (appConfig ^. registry)
    , _clientConfigDTOQuestionnaire = toClientConfigQuestionnaireDTO $ appConfig ^. questionnaire
    , _clientConfigDTOTemplate = appConfig ^. template
    , _clientConfigDTOSubmission = SimpleFeature $ appConfig ^. submission . enabled
    , _clientConfigDTOCloud = toClientConfigCloudDTO (serverConfig ^. cloud) app
    , _clientConfigDTOLocales = fmap toDTO locales
    , _clientConfigDTOOwl = appConfig ^. owl
    }

toClientAuthDTO :: AppConfigAuth -> ClientConfigAuthDTO
toClientAuthDTO config =
  ClientConfigAuthDTO
    { _clientConfigAuthDTODefaultRole = config ^. defaultRole
    , _clientConfigAuthDTOInternal = config ^. internal
    , _clientConfigAuthDTOExternal = toClientAuthExternalDTO $ config ^. external
    }

toClientAuthExternalDTO :: AppConfigAuthExternal -> ClientConfigAuthExternalDTO
toClientAuthExternalDTO config =
  ClientConfigAuthExternalDTO
    {_clientConfigAuthExternalDTOServices = toClientAuthExternalServiceDTO <$> config ^. services}

toClientAuthExternalServiceDTO :: AppConfigAuthExternalService -> ClientConfigAuthExternalServiceDTO
toClientAuthExternalServiceDTO config =
  ClientConfigAuthExternalServiceDTO
    { _clientConfigAuthExternalServiceDTOAId = config ^. aId
    , _clientConfigAuthExternalServiceDTOName = config ^. name
    , _clientConfigAuthExternalServiceDTOUrl = config ^. url
    , _clientConfigAuthExternalServiceDTOStyle = config ^. style
    }

toClientConfigRegistryDTO :: ServerConfigRegistry -> AppConfigRegistry -> ClientConfigRegistryDTO
toClientConfigRegistryDTO serverConfig appConfig =
  ClientConfigRegistryDTO
    {_clientConfigRegistryDTOEnabled = appConfig ^. enabled, _clientConfigRegistryDTOUrl = serverConfig ^. clientUrl}

toClientConfigQuestionnaireDTO :: AppConfigQuestionnaire -> ClientConfigQuestionnaireDTO
toClientConfigQuestionnaireDTO appConfig =
  ClientConfigQuestionnaireDTO
    { _clientConfigQuestionnaireDTOQuestionnaireVisibility = appConfig ^. questionnaireVisibility
    , _clientConfigQuestionnaireDTOQuestionnaireSharing = appConfig ^. questionnaireSharing
    , _clientConfigQuestionnaireDTOQuestionnaireCreation = appConfig ^. questionnaireCreation
    , _clientConfigQuestionnaireDTOProjectTagging = SimpleFeature $ appConfig ^. projectTagging . enabled
    , _clientConfigQuestionnaireDTOSummaryReport = appConfig ^. summaryReport
    , _clientConfigQuestionnaireDTOFeedback = SimpleFeature $ appConfig ^. feedback . enabled
    }

toClientConfigCloudDTO :: ServerConfigCloud -> App -> ClientConfigCloudDTO
toClientConfigCloudDTO serverConfig app =
  ClientConfigCloudDTO
    {_clientConfigCloudDTOEnabled = serverConfig ^. enabled, _clientConfigCloudDTOServerUrl = app ^. serverUrl}
