module Api.Resource.Config.ClientConfigDTO where

data ClientConfigDTO = ClientConfigDTO
  { _clientConfigDTOClient :: ClientConfigClientDTO
  , _clientConfigDTOFeedbackEnabled :: Bool
  , _clientConfigDTORegistrationEnabled :: Bool
  , _clientConfigDTOPublicQuestionnaireEnabled :: Bool
  , _clientConfigDTOLevelsEnabled :: Bool
  , _clientConfigDTOItemTitleEnabled :: Bool
  , _clientConfigDTOQuestionnaireAccessibilityEnabled :: Bool
  , _clientConfigDTORegistry :: ClientConfigRegistryDTO
  } deriving (Show, Eq)

data ClientConfigRegistryDTO = ClientConfigRegistryDTO
  { _clientConfigRegistryEnabled :: Bool
  , _clientConfigRegistryUrl :: String
  } deriving (Show, Eq)

data ClientConfigClientDTO = ClientConfigClientDTO
  { _clientConfigClientPrivacyUrl :: String
  , _clientConfigClientAppTitle :: Maybe String
  , _clientConfigClientAppTitleShort :: Maybe String
  , _clientConfigClientWelcomeWarning :: Maybe String
  , _clientConfigClientWelcomeInfo :: Maybe String
  , _clientConfigClientDashboard :: Maybe ClientConfigClientDashboardDTO
  } deriving (Show, Eq)

data ClientConfigClientDashboardDTO = ClientConfigClientDashboardDTO
  { _clientConfigClientDashboardAdmin :: [String]
  , _clientConfigClientDashboardDataSteward :: [String]
  , _clientConfigClientDashboardResearcher :: [String]
  } deriving (Show, Eq)
