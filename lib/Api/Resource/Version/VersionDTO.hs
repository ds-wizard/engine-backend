module Api.Resource.Version.VersionDTO where

import Control.Monad
import Data.Aeson

data VersionDTO = VersionDTO
  { _versionDTODescription :: String
  }

instance FromJSON VersionDTO where
  parseJSON (Object o) = do
    _versionDTODescription <- o .: "description"
    return VersionDTO {..}
  parseJSON _ = mzero

instance ToJSON VersionDTO where
  toJSON VersionDTO {..} = object ["description" .= _versionDTODescription]
