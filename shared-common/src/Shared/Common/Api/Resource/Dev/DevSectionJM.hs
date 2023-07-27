module Shared.Common.Api.Resource.Dev.DevSectionJM where

import Data.Aeson

import Shared.Common.Api.Resource.Dev.DevJM ()
import Shared.Common.Api.Resource.Dev.DevOperationJM ()
import Shared.Common.Api.Resource.Dev.DevSectionDTO
import Shared.Common.Util.Aeson

instance FromJSON DevSectionDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DevSectionDTO where
  toJSON = genericToJSON jsonOptions
