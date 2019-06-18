module Service.Config.ClientConfigMapper where

import Control.Lens ((^.))

import Api.Resource.Config.ClientConfigDTO
import LensesConfig
import Model.Config.AppConfig

toClientConfigDTO :: AppConfig -> ClientConfigDTO
toClientConfigDTO appConfig =
  ClientConfigDTO
  { _clientConfigDTOClient = toClientConfigClientDTO (appConfig ^. client)
  , _clientConfigDTOFeedbackEnabled = appConfig ^. feedback . enabled
  , _clientConfigDTORegistrationEnabled = appConfig ^. general . registrationEnabled
  , _clientConfigDTOPublicQuestionnaireEnabled = appConfig ^. general . publicQuestionnaireEnabled
  , _clientConfigDTOLevelsEnabled = appConfig ^. general . levelsEnabled
  , _clientConfigDTOItemTitleEnabled = appConfig ^. general . itemTitleEnabled
  , _clientConfigDTOQuestionnaireAccessibilityEnabled = appConfig ^. general . questionnaireAccessibilityEnabled
  , _clientConfigDTORegistry = toClientConfigRegistryDTO (appConfig ^. registry)
  }

toClientConfigRegistryDTO :: AppConfigRegistry -> ClientConfigRegistryDTO
toClientConfigRegistryDTO registryConfig =
  ClientConfigRegistryDTO
  {_clientConfigRegistryEnabled = registryConfig ^. enabled, _clientConfigRegistryUrl = registryConfig ^. clientUrl}

toClientConfigClientDTO :: AppConfigClient -> ClientConfigClientDTO
toClientConfigClientDTO clientConfig =
  ClientConfigClientDTO
  { _clientConfigClientPrivacyUrl = clientConfig ^. privacyUrl
  , _clientConfigClientAppTitle = clientConfig ^. appTitle
  , _clientConfigClientAppTitleShort = clientConfig ^. appTitleShort
  , _clientConfigClientWelcomeWarning = clientConfig ^. welcomeWarning
  , _clientConfigClientWelcomeInfo = clientConfig ^. welcomeInfo
  , _clientConfigClientDashboard = toClientConfigClientDashboardDTO <$> clientConfig ^. dashboard
  }

toClientConfigClientDashboardDTO :: AppConfigClientDashboard -> ClientConfigClientDashboardDTO
toClientConfigClientDashboardDTO dashboardConfig =
  ClientConfigClientDashboardDTO
  { _clientConfigClientDashboardAdmin = dashboardConfig ^. admin
  , _clientConfigClientDashboardDataSteward = dashboardConfig ^. dataSteward
  , _clientConfigClientDashboardResearcher = dashboardConfig ^. researcher
  }
