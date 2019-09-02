module Api.Resource.Config.ClientConfigJM where

import Data.Aeson

import Api.Resource.Config.ClientConfigDTO
import Api.Resource.Event.EventJM ()
import Util.JSON (simpleParseJSON, simpleToJSON)

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
  parseJSON = simpleParseJSON "_clientConfigClientDashboardDTO"

instance ToJSON ClientConfigClientDashboardDTO where
  toJSON = simpleToJSON "_clientConfigClientDashboardDTO"

instance FromJSON ClientConfigClientCustomMenuLinkDTO where
  parseJSON = simpleParseJSON "_clientConfigClientCustomMenuLinkDTO"

instance ToJSON ClientConfigClientCustomMenuLinkDTO where
  toJSON = simpleToJSON "_clientConfigClientCustomMenuLinkDTO"
