module Api.Resource.Version.VersionDTO where

import Control.Monad
import Data.Aeson

data VersionDTO = VersionDTO
  { _versionDTODescription :: String
  , _versionDTOReadme :: String
  }

instance FromJSON VersionDTO where
  parseJSON (Object o) = do
    _versionDTODescription <- o .: "description"
    _versionDTOReadme <- o .: "readme"
    return VersionDTO {..}
  parseJSON _ = mzero

instance ToJSON VersionDTO where
  toJSON VersionDTO {..} = object ["description" .= _versionDTODescription, "readme" .= _versionDTOReadme]
