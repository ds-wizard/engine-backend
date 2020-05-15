module Wizard.Service.Config.ClientConfigMapper where

import Control.Lens ((^.))

import LensesConfig
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Config.SimpleFeature

toClientConfigDTO :: ServerConfig -> AppConfig -> ClientConfigDTO
toClientConfigDTO serverConfig appConfig =
  ClientConfigDTO
    { _clientConfigDTOOrganization = appConfig ^. organization
    , _clientConfigDTOAuthentication = toClientAuthDTO $ appConfig ^. authentication
    , _clientConfigDTOPrivacyAndSupport = appConfig ^. privacyAndSupport
    , _clientConfigDTODashboard = appConfig ^. dashboard
    , _clientConfigDTOLookAndFeel = appConfig ^. lookAndFeel
    , _clientConfigDTOKnowledgeModelRegistry =
        toClientConfigRegistryDTO (serverConfig ^. registry) (appConfig ^. knowledgeModelRegistry)
    , _clientConfigDTOQuestionnaire = toClientConfigQuestionnaireDTO $ appConfig ^. questionnaire
    , _clientConfigDTOTemplate = appConfig ^. template
    , _clientConfigDTOSubmission = SimpleFeature $ appConfig ^. submission . enabled
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
    { _clientConfigQuestionnaireDTOQuestionnaireAccessibility = appConfig ^. questionnaireAccessibility
    , _clientConfigQuestionnaireDTOLevels = appConfig ^. levels
    , _clientConfigQuestionnaireDTOFeedback = SimpleFeature $ appConfig ^. feedback . enabled
    }
