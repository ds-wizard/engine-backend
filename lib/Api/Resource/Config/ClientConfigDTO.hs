module Api.Resource.Config.ClientConfigDTO where

data ClientConfigDTO = ClientConfigDTO
  { _clientConfigDTOClient :: ClientConfigClientDTO
  , _clientConfigDTOFeedbackEnabled :: Bool
  , _clientConfigDTORegistrationEnabled :: Bool
  , _clientConfigDTOPublicQuestionnaireEnabled :: Bool
  , _clientConfigDTOLevelsEnabled :: Bool
  } deriving (Show, Eq)

data ClientConfigClientDTO = ClientConfigClientDTO
  { _clientConfigAppTitle :: Maybe String
  , _clientConfigAppTitleShort :: Maybe String
  , _clientConfigWelcomeWarning :: Maybe String
  , _clientConfigWelcomeInfo :: Maybe String
  , _clientConfigDashboard :: Maybe ClientConfigClientDashboardDTO
  } deriving (Show, Eq)

data ClientConfigClientDashboardDTO = ClientConfigClientDashboardDTO
  { _clientConfigClientDashboardAdmin :: Maybe [String]
  , _clientConfigClientDashboardDataSteward :: Maybe [String]
  , _clientConfigClientDashboardResearcher :: Maybe [String]
  } deriving (Show, Eq)
