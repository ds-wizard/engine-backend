module Service.Config.ClientConfigMapper where

import Control.Lens ((^.))

import Api.Resource.Config.ClientConfigDTO
import LensesConfig
import Model.Config.AppConfig

toDTO :: AppConfig -> ClientConfigDTO
toDTO appConfig =
  ClientConfigDTO
  { _clientConfigDTOClient =
      ClientConfigClientDTO
      { _clientConfigAppTitle = appConfig ^. client . appTitle
      , _clientConfigAppTitleShort = appConfig ^. client . appTitleShort
      , _clientConfigWelcomeWarning = appConfig ^. client . welcomeInfo
      , _clientConfigWelcomeInfo = appConfig ^. client . welcomeWarning
      }
  , _clientConfigDTOFeedbackEnabled = appConfig ^. feedback . enabled
  , _clientConfigDTORegistrationEnabled = appConfig ^. general . registrationEnabled
  , _clientConfigDTOPublicQuestionnaireEnabled = appConfig ^. general . publicQuestionnaireEnabled
  , _clientConfigDTOLevelsEnabled = appConfig ^. general . levelsEnabled
  }
