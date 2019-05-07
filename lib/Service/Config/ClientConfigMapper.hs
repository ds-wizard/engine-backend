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
  }

toClientConfigClientDTO :: AppConfigClient -> ClientConfigClientDTO
toClientConfigClientDTO clientConfig =
  ClientConfigClientDTO
  { _clientConfigAppTitle = clientConfig ^. appTitle
  , _clientConfigAppTitleShort = clientConfig ^. appTitleShort
  , _clientConfigWelcomeWarning = clientConfig ^. welcomeInfo
  , _clientConfigWelcomeInfo = clientConfig ^. welcomeWarning
  , _clientConfigDashboard = toClientConfigClientDashboardDTO <$> clientConfig ^. dashboard
  }

toClientConfigClientDashboardDTO :: AppConfigClientDashboard -> ClientConfigClientDashboardDTO
toClientConfigClientDashboardDTO dashboardConfig =
  ClientConfigClientDashboardDTO
  { _clientConfigClientDashboardAdmin = dashboardConfig ^. admin
  , _clientConfigClientDashboardDataSteward = dashboardConfig ^. dataSteward
  , _clientConfigClientDashboardResearcher = dashboardConfig ^. researcher
  }
