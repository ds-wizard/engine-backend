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
  { _clientConfigRegistryDTOEnabled = registryConfig ^. enabled
  , _clientConfigRegistryDTOUrl = registryConfig ^. clientUrl
  }

toClientConfigClientDTO :: AppConfigClient -> ClientConfigClientDTO
toClientConfigClientDTO clientConfig =
  ClientConfigClientDTO
  { _clientConfigClientDTOPrivacyUrl = clientConfig ^. privacyUrl
  , _clientConfigClientDTOAppTitle = clientConfig ^. appTitle
  , _clientConfigClientDTOAppTitleShort = clientConfig ^. appTitleShort
  , _clientConfigClientDTOWelcomeWarning = clientConfig ^. welcomeWarning
  , _clientConfigClientDTOWelcomeInfo = clientConfig ^. welcomeInfo
  , _clientConfigClientDTODashboard = toClientConfigClientDashboardDTO <$> clientConfig ^. dashboard
  , _clientConfigClientDTOCustomMenuLinks = toClientConfigClientCustomMenuLinksDTO <$> clientConfig ^. customMenuLinks
  }

toClientConfigClientDashboardDTO :: AppConfigClientDashboard -> ClientConfigClientDashboardDTO
toClientConfigClientDashboardDTO dashboardConfig =
  ClientConfigClientDashboardDTO
  { _clientConfigClientDashboardDTOAdmin = dashboardConfig ^. admin
  , _clientConfigClientDashboardDTODataSteward = dashboardConfig ^. dataSteward
  , _clientConfigClientDashboardDTOResearcher = dashboardConfig ^. researcher
  }

toClientConfigClientCustomMenuLinksDTO :: AppConfigClientCustomMenuLink -> ClientConfigClientCustomMenuLinkDTO
toClientConfigClientCustomMenuLinksDTO customMenuLinkConfig =
  ClientConfigClientCustomMenuLinkDTO
  { _clientConfigClientCustomMenuLinkDTOIcon = customMenuLinkConfig ^. icon
  , _clientConfigClientCustomMenuLinkDTOTitle = customMenuLinkConfig ^. title
  , _clientConfigClientCustomMenuLinkDTOUrl = customMenuLinkConfig ^. url
  , _clientConfigClientCustomMenuLinkDTONewWindow = customMenuLinkConfig ^. newWindow
  }
