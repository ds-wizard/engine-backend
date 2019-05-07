module Api.Resource.Config.ClientConfigJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Config.ClientConfigDTO

instance FromJSON ClientConfigDTO where
  parseJSON (Object o) = do
    _clientConfigDTOClient <- o .: "client"
    _clientConfigDTOFeedbackEnabled <- o .: "feedbackEnabled"
    _clientConfigDTORegistrationEnabled <- o .: "registrationEnabled"
    _clientConfigDTOPublicQuestionnaireEnabled <- o .: "publicQuestionnaireEnabled"
    _clientConfigDTOLevelsEnabled <- o .: "levelsEnabled"
    _clientConfigDTOItemTitleEnabled <- o .: "itemTitleEnabled"
    return ClientConfigDTO {..}
  parseJSON _ = mzero

instance ToJSON ClientConfigDTO where
  toJSON ClientConfigDTO {..} =
    object
      [ "client" .= _clientConfigDTOClient
      , "feedbackEnabled" .= _clientConfigDTOFeedbackEnabled
      , "registrationEnabled" .= _clientConfigDTORegistrationEnabled
      , "publicQuestionnaireEnabled" .= _clientConfigDTOPublicQuestionnaireEnabled
      , "levelsEnabled" .= _clientConfigDTOLevelsEnabled
      , "itemTitleEnabled" .= _clientConfigDTOItemTitleEnabled
      ]

instance FromJSON ClientConfigClientDTO where
  parseJSON (Object o) = do
    _clientConfigAppTitle <- o .: "appTitle"
    _clientConfigAppTitleShort <- o .: "appTitleShort"
    _clientConfigWelcomeWarning <- o .: "welcomeWarning"
    _clientConfigWelcomeInfo <- o .: "welcomeInfo"
    _clientConfigDashboard <- o .: "dashboard"
    return ClientConfigClientDTO {..}
  parseJSON _ = mzero

instance ToJSON ClientConfigClientDTO where
  toJSON ClientConfigClientDTO {..} =
    object
      [ "appTitle" .= _clientConfigAppTitle
      , "appTitleShort" .= _clientConfigAppTitleShort
      , "welcomeWarning" .= _clientConfigWelcomeWarning
      , "welcomeInfo" .= _clientConfigWelcomeInfo
      , "dashboard" .= _clientConfigDashboard
      ]

instance FromJSON ClientConfigClientDashboardDTO where
  parseJSON (Object o) = do
    _clientConfigClientDashboardAdmin <- o .: "ADMIN"
    _clientConfigClientDashboardDataSteward <- o .: "DATASTEWARD"
    _clientConfigClientDashboardResearcher <- o .: "RESEARCHER"
    return ClientConfigClientDashboardDTO {..}
  parseJSON _ = mzero

instance ToJSON ClientConfigClientDashboardDTO where
  toJSON ClientConfigClientDashboardDTO {..} =
    object
      [ "ADMIN" .= _clientConfigClientDashboardAdmin
      , "DATASTEWARD" .= _clientConfigClientDashboardDataSteward
      , "RESEARCHER" .= _clientConfigClientDashboardResearcher
      ]
