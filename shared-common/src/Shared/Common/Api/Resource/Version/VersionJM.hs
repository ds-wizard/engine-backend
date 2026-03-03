module Shared.Common.Api.Resource.Version.VersionJM where

import Data.Aeson

import Shared.Common.Api.Resource.Version.VersionDTO
import Shared.Common.Util.Aeson

instance FromJSON VersionDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON VersionDTO where
  toJSON = genericToJSON jsonOptions
