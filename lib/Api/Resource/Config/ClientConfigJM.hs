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
    return ClientConfigDTO {..}
  parseJSON _ = mzero

instance ToJSON ClientConfigDTO where
  toJSON ClientConfigDTO {..} =
    object
      [ "client" .= _clientConfigDTOClient
      , "feedbackEnabled" .= _clientConfigDTOFeedbackEnabled
      , "registrationEnabled" .= _clientConfigDTORegistrationEnabled
      , "publicQuestionnaireEnabled" .= _clientConfigDTOPublicQuestionnaireEnabled
      ]

instance FromJSON ClientConfigClientDTO where
  parseJSON (Object o) = do
    _clientConfigAppTitle <- o .: "appTitle"
    _clientConfigAppTitleShort <- o .: "appTitleShort"
    _clientConfigWelcomeWarning <- o .: "welcomeWarning"
    _clientConfigWelcomeInfo <- o .: "welcomeInfo"
    return ClientConfigClientDTO {..}
  parseJSON _ = mzero

instance ToJSON ClientConfigClientDTO where
  toJSON ClientConfigClientDTO {..} =
    object
      [ "appTitle" .= _clientConfigAppTitle
      , "appTitleShort" .= _clientConfigAppTitleShort
      , "welcomeWarning" .= _clientConfigWelcomeWarning
      , "welcomeInfo" .= _clientConfigWelcomeInfo
      ]
