module Api.Resource.Config.ClientConfigDTO where

import GHC.Generics

data ClientConfigDTO = ClientConfigDTO
  { _clientConfigDTOClient :: ClientConfigClientDTO
  , _clientConfigDTOFeedbackEnabled :: Bool
  , _clientConfigDTORegistrationEnabled :: Bool
  , _clientConfigDTOPublicQuestionnaireEnabled :: Bool
  , _clientConfigDTOLevelsEnabled :: Bool
  , _clientConfigDTOItemTitleEnabled :: Bool
  , _clientConfigDTOQuestionnaireAccessibilityEnabled :: Bool
  , _clientConfigDTORegistry :: ClientConfigRegistryDTO
  } deriving (Show, Eq, Generic)

data ClientConfigRegistryDTO = ClientConfigRegistryDTO
  { _clientConfigRegistryDTOEnabled :: Bool
  , _clientConfigRegistryDTOUrl :: String
  } deriving (Show, Eq, Generic)

data ClientConfigClientDTO = ClientConfigClientDTO
  { _clientConfigClientDTOPrivacyUrl :: String
  , _clientConfigClientDTOAppTitle :: Maybe String
  , _clientConfigClientDTOAppTitleShort :: Maybe String
  , _clientConfigClientDTOWelcomeWarning :: Maybe String
  , _clientConfigClientDTOWelcomeInfo :: Maybe String
  , _clientConfigClientDTODashboard :: Maybe ClientConfigClientDashboardDTO
  , _clientConfigClientDTOCustomMenuLinks :: [ClientConfigClientCustomMenuLinkDTO]
  } deriving (Show, Eq, Generic)

data ClientConfigClientDashboardDTO = ClientConfigClientDashboardDTO
  { _clientConfigClientDashboardDTOAdmin :: [String]
  , _clientConfigClientDashboardDTODataSteward :: [String]
  , _clientConfigClientDashboardDTOResearcher :: [String]
  } deriving (Show, Eq, Generic)

data ClientConfigClientCustomMenuLinkDTO = ClientConfigClientCustomMenuLinkDTO
  { _clientConfigClientCustomMenuLinkDTOIcon :: String
  , _clientConfigClientCustomMenuLinkDTOTitle :: String
  , _clientConfigClientCustomMenuLinkDTOUrl :: String
  , _clientConfigClientCustomMenuLinkDTONewWindow :: Bool
  } deriving (Show, Eq, Generic)
