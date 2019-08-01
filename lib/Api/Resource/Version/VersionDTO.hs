module Api.Resource.Version.VersionDTO where

import GHC.Generics

data VersionDTO = VersionDTO
  { _versionDTODescription :: String
  , _versionDTOReadme :: String
  , _versionDTOLicense :: String
  } deriving (Generic)
