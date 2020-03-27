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

instance FromJSON AppConfigAuthDTO where
  parseJSON = simpleParseJSON "_appConfigAuthDTO"

instance ToJSON AppConfigAuthDTO where
  toJSON = simpleToJSON "_appConfigAuthDTO"

instance FromJSON AppConfigAuthInternalDTO where
  parseJSON = simpleParseJSON "_appConfigAuthInternalDTO"

instance ToJSON AppConfigAuthInternalDTO where
  toJSON = simpleToJSON "_appConfigAuthInternalDTO"

instance FromJSON AppConfigAuthExternalDTO where
  parseJSON = simpleParseJSON "_appConfigAuthExternalDTO"

instance ToJSON AppConfigAuthExternalDTO where
  toJSON = simpleToJSON "_appConfigAuthExternalDTO"

instance FromJSON AppConfigAuthExternalServiceDTO where
  parseJSON = simpleParseJSON "_appConfigAuthExternalServiceDTO"

instance ToJSON AppConfigAuthExternalServiceDTO where
  toJSON = simpleToJSON "_appConfigAuthExternalServiceDTO"

instance FromJSON AppConfigAuthExternalServiceParameterDTO where
  parseJSON = simpleParseJSON "_appConfigAuthExternalServiceParameterDTO"

instance ToJSON AppConfigAuthExternalServiceParameterDTO where
  toJSON = simpleToJSON "_appConfigAuthExternalServiceParameterDTO"

instance FromJSON AppConfigAuthExternalServiceStyleDTO where
  parseJSON = simpleParseJSON "_appConfigAuthExternalServiceStyleDTO"

instance ToJSON AppConfigAuthExternalServiceStyleDTO where
  toJSON = simpleToJSON "_appConfigAuthExternalServiceStyleDTO"
