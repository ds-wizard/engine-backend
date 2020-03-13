module Wizard.Api.Resource.Config.ClientConfigJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Config.AppConfigJM ()
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Api.Resource.Config.SimpleFeatureJM ()

instance FromJSON ClientConfigDTO where
  parseJSON = simpleParseJSON "_clientConfigDTO"

instance ToJSON ClientConfigDTO where
  toJSON = simpleToJSON "_clientConfigDTO"

instance FromJSON ClientConfigFeaturesDTO where
  parseJSON = simpleParseJSON "_clientConfigFeaturesDTO"

instance ToJSON ClientConfigFeaturesDTO where
  toJSON = simpleToJSON "_clientConfigFeaturesDTO"

instance FromJSON ClientConfigRegistryDTO where
  parseJSON = simpleParseJSON "_clientConfigRegistryDTO"

instance ToJSON ClientConfigRegistryDTO where
  toJSON = simpleToJSON "_clientConfigRegistryDTO"
