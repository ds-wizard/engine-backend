module Shared.Common.Api.Resource.Dev.DevOperationJM where

import Data.Aeson

import Shared.Common.Api.Resource.Dev.DevJM ()
import Shared.Common.Api.Resource.Dev.DevOperationDTO
import Shared.Common.Util.Aeson

instance FromJSON DevOperationDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DevOperationDTO where
  toJSON = genericToJSON jsonOptions
