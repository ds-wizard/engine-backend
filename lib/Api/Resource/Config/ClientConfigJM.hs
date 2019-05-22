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
    _clientConfigDTORegistry <- o .: "registry"
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
      , "registry" .= _clientConfigDTORegistry
      ]

instance FromJSON ClientConfigRegistryDTO where
  parseJSON (Object o) = do
    _clientConfigRegistryEnabled <- o .: "enabled"
    _clientConfigRegistryUrl <- o .: "url"
    return ClientConfigRegistryDTO {..}
  parseJSON _ = mzero

instance ToJSON ClientConfigRegistryDTO where
  toJSON ClientConfigRegistryDTO {..} =
    object ["enabled" .= _clientConfigRegistryEnabled, "url" .= _clientConfigRegistryUrl]

instance FromJSON ClientConfigClientDTO where
  parseJSON (Object o) = do
    _clientConfigClientAppTitle <- o .: "appTitle"
    _clientConfigClientAppTitleShort <- o .: "appTitleShort"
    _clientConfigClientWelcomeWarning <- o .: "welcomeWarning"
    _clientConfigClientWelcomeInfo <- o .: "welcomeInfo"
    _clientConfigClientDashboard <- o .: "dashboard"
    return ClientConfigClientDTO {..}
  parseJSON _ = mzero

instance ToJSON ClientConfigClientDTO where
  toJSON ClientConfigClientDTO {..} =
    object
      [ "appTitle" .= _clientConfigClientAppTitle
      , "appTitleShort" .= _clientConfigClientAppTitleShort
      , "welcomeWarning" .= _clientConfigClientWelcomeWarning
      , "welcomeInfo" .= _clientConfigClientWelcomeInfo
      , "dashboard" .= _clientConfigClientDashboard
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
