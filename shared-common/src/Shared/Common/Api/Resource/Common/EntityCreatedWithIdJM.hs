module Shared.Common.Api.Resource.Common.EntityCreatedWithIdJM where

import Data.Aeson

import Shared.Common.Api.Resource.Common.EntityCreatedWithIdDTO
import Shared.Common.Util.Aeson

instance FromJSON EntityCreatedWithIdDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON EntityCreatedWithIdDTO where
  toJSON = genericToJSON jsonOptions
