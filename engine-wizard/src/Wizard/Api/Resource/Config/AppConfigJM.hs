module Wizard.Api.Resource.Config.AppConfigJM where

import Control.Monad (mzero)
import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Config.AppConfigDTO
import Wizard.Api.Resource.Config.SimpleFeatureJM ()

instance FromJSON AppConfigFeaturesDTO where
  parseJSON = simpleParseJSON "_appConfigFeaturesDTO"

instance ToJSON AppConfigFeaturesDTO where
  toJSON = simpleToJSON "_appConfigFeaturesDTO"

instance FromJSON AppConfigClientDTO where
  parseJSON = simpleParseJSON "_appConfigClientDTO"

instance ToJSON AppConfigClientDTO where
  toJSON = simpleToJSON "_appConfigClientDTO"

instance FromJSON AppConfigClientDashboardDTO where
  parseJSON (Object o) = do
    _appConfigClientDashboardDTOAdmin <- o .: "ADMIN"
    _appConfigClientDashboardDTODataSteward <- o .: "DATASTEWARD"
    _appConfigClientDashboardDTOResearcher <- o .: "RESEARCHER"
    return AppConfigClientDashboardDTO {..}
  parseJSON _ = mzero

instance ToJSON AppConfigClientDashboardDTO where
  toJSON AppConfigClientDashboardDTO {..} =
    object
      [ "ADMIN" .= _appConfigClientDashboardDTOAdmin
      , "DATASTEWARD" .= _appConfigClientDashboardDTODataSteward
      , "RESEARCHER" .= _appConfigClientDashboardDTOResearcher
      ]

instance FromJSON AppConfigClientCustomMenuLinkDTO where
  parseJSON = simpleParseJSON "_appConfigClientCustomMenuLinkDTO"

instance ToJSON AppConfigClientCustomMenuLinkDTO where
  toJSON = simpleToJSON "_appConfigClientCustomMenuLinkDTO"

instance FromJSON AppConfigInfoDTO where
  parseJSON = simpleParseJSON "_appConfigInfoDTO"

instance ToJSON AppConfigInfoDTO where
  toJSON = simpleToJSON "_appConfigInfoDTO"

instance FromJSON AppConfigAffiliationDTO where
  parseJSON = simpleParseJSON "_appConfigAffiliationDTO"

instance ToJSON AppConfigAffiliationDTO where
  toJSON = simpleToJSON "_appConfigAffiliationDTO"
