module Api.Resource.Version.VersionDTO where

import Control.Monad
import Data.Aeson

data VersionDTO = VersionDTO
  { _versionDTODescription :: String
  , _versionDTOReadme :: String
  , _versionDTOLicense :: String
  }

instance FromJSON VersionDTO where
  parseJSON (Object o) = do
    _versionDTODescription <- o .: "description"
    _versionDTOReadme <- o .: "readme"
    _versionDTOLicense <- o .: "license"
    return VersionDTO {..}
  parseJSON _ = mzero

instance ToJSON VersionDTO where
  toJSON VersionDTO {..} =
    object ["description" .= _versionDTODescription, "readme" .= _versionDTOReadme, "license" .= _versionDTOLicense]
