module Shared.Common.Api.Resource.Dev.DevExecutionResultJM where

import Data.Aeson

import Shared.Common.Api.Resource.Dev.DevExecutionResultDTO
import Shared.Common.Util.Aeson

instance FromJSON AdminExecutionResultDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AdminExecutionResultDTO where
  toJSON = genericToJSON jsonOptions
