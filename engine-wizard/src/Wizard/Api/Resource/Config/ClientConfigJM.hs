module Wizard.Api.Resource.Config.ClientConfigJM where

import Control.Monad (mzero)
import Data.Aeson

import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Api.Resource.Event.EventJM ()
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON ClientConfigDTO where
  parseJSON = simpleParseJSON "_clientConfigDTO"

instance ToJSON ClientConfigDTO where
  toJSON = simpleToJSON "_clientConfigDTO"

instance FromJSON ClientConfigRegistryDTO where
  parseJSON = simpleParseJSON "_clientConfigRegistryDTO"

instance ToJSON ClientConfigRegistryDTO where
  toJSON = simpleToJSON "_clientConfigRegistryDTO"

instance FromJSON ClientConfigClientDTO where
  parseJSON = simpleParseJSON "_clientConfigClientDTO"

instance ToJSON ClientConfigClientDTO where
  toJSON = simpleToJSON "_clientConfigClientDTO"

instance FromJSON ClientConfigClientDashboardDTO where
  parseJSON (Object o) = do
    _clientConfigClientDashboardDTOAdmin <- o .: "ADMIN"
    _clientConfigClientDashboardDTODataSteward <- o .: "DATASTEWARD"
    _clientConfigClientDashboardDTOResearcher <- o .: "RESEARCHER"
    return ClientConfigClientDashboardDTO {..}
  parseJSON _ = mzero

instance ToJSON ClientConfigClientDashboardDTO where
  toJSON ClientConfigClientDashboardDTO {..} =
    object
      [ "ADMIN" .= _clientConfigClientDashboardDTOAdmin
      , "DATASTEWARD" .= _clientConfigClientDashboardDTODataSteward
      , "RESEARCHER" .= _clientConfigClientDashboardDTOResearcher
      ]

instance FromJSON ClientConfigClientCustomMenuLinkDTO where
  parseJSON = simpleParseJSON "_clientConfigClientCustomMenuLinkDTO"

instance ToJSON ClientConfigClientCustomMenuLinkDTO where
  toJSON = simpleToJSON "_clientConfigClientCustomMenuLinkDTO"
