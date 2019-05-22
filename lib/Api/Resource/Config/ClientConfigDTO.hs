module Api.Resource.Config.ClientConfigDTO where

data ClientConfigDTO = ClientConfigDTO
  { _clientConfigDTOClient :: ClientConfigClientDTO
  , _clientConfigDTOFeedbackEnabled :: Bool
  , _clientConfigDTORegistrationEnabled :: Bool
  , _clientConfigDTOPublicQuestionnaireEnabled :: Bool
  , _clientConfigDTOLevelsEnabled :: Bool
  , _clientConfigDTOItemTitleEnabled :: Bool
  , _clientConfigDTORegistry :: ClientConfigRegistryDTO
  } deriving (Show, Eq)

data ClientConfigRegistryDTO = ClientConfigRegistryDTO
  { _clientConfigRegistryEnabled :: Bool
  , _clientConfigRegistryUrl :: Maybe String
  } deriving (Show, Eq)

data ClientConfigClientDTO = ClientConfigClientDTO
  { _clientConfigClientAppTitle :: Maybe String
  , _clientConfigClientAppTitleShort :: Maybe String
  , _clientConfigClientWelcomeWarning :: Maybe String
  , _clientConfigClientWelcomeInfo :: Maybe String
  , _clientConfigClientDashboard :: Maybe ClientConfigClientDashboardDTO
  } deriving (Show, Eq)

data ClientConfigClientDashboardDTO = ClientConfigClientDashboardDTO
  { _clientConfigClientDashboardAdmin :: Maybe [String]
  , _clientConfigClientDashboardDataSteward :: Maybe [String]
  , _clientConfigClientDashboardResearcher :: Maybe [String]
  } deriving (Show, Eq)
