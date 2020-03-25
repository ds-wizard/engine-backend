module Wizard.Service.Config.ClientConfigMapper where

import Control.Lens ((^.))

import LensesConfig
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Service.Config.AppConfigMapper
import Wizard.Service.Config.SimpleFeatureMapper

toClientConfigDTO :: ServerConfig -> AppConfig -> ClientConfigDTO
toClientConfigDTO serverConfig appConfig =
  ClientConfigDTO
    { _clientConfigDTOFeatures = toClientConfigFeaturesDTO serverConfig appConfig
    , _clientConfigDTOClient = toClientDTO (appConfig ^. client)
    , _clientConfigDTOInfo = toInfoDTO (appConfig ^. info)
    , _clientConfigDTOAffiliation = toAffiliationDTO (appConfig ^. affiliation)
    }

toClientConfigFeaturesDTO :: ServerConfig -> AppConfig -> ClientConfigFeaturesDTO
toClientConfigFeaturesDTO serverConfig appConfig =
  ClientConfigFeaturesDTO
    { _clientConfigFeaturesDTORegistration = toSimpleFeatureDTO $ appConfig ^. features . registration
    , _clientConfigFeaturesDTOPublicQuestionnaire = toSimpleFeatureDTO $ appConfig ^. features . publicQuestionnaire
    , _clientConfigFeaturesDTOLevels = toSimpleFeatureDTO $ appConfig ^. features . levels
    , _clientConfigFeaturesDTOQuestionnaireAccessibility =
        toSimpleFeatureDTO $ appConfig ^. features . questionnaireAccessibility
    , _clientConfigFeaturesDTOFeedback = toSimpleFeatureDTO $ serverConfig ^. feedback
    , _clientConfigFeaturesDTORegistry = toClientConfigRegistryDTO (serverConfig ^. registry)
    }

toClientConfigRegistryDTO :: ServerConfigRegistry -> ClientConfigRegistryDTO
toClientConfigRegistryDTO registryConfig =
  ClientConfigRegistryDTO
    { _clientConfigRegistryDTOEnabled = registryConfig ^. enabled
    , _clientConfigRegistryDTOUrl = registryConfig ^. clientUrl
    }
