module Wizard.Service.Config.ClientConfigMapper where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Config.ServerConfig
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Config.SimpleFeature

toClientConfigDTO :: ServerConfig -> AppConfig -> ClientConfigDTO
toClientConfigDTO serverConfig appConfig =
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
    , _clientConfigDTOExperimental = toClientConfigExperimentalDTO $ serverConfig ^. experimental
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
    , _clientConfigQuestionnaireDTOSummaryReport = appConfig ^. summaryReport
    , _clientConfigQuestionnaireDTOFeedback = SimpleFeature $ appConfig ^. feedback . enabled
    }

toClientConfigExperimentalDTO :: ServerConfigExperimental -> ClientConfigExperimentalDTO
toClientConfigExperimentalDTO serverConfig =
  ClientConfigExperimentalDTO {_clientConfigExperimentalDTOMoreAppsEnabled = serverConfig ^. moreAppsEnabled}
