module Shared.Common.Api.Resource.Dev.DevExecutionJM where

import Data.Aeson

import Shared.Common.Api.Resource.Dev.DevExecutionDTO
import Shared.Common.Util.Aeson

instance FromJSON DevExecutionDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DevExecutionDTO where
  toJSON = genericToJSON jsonOptions
