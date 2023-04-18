module Shared.Common.Api.Resource.Common.PageMetadataJM where

import Data.Aeson

import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Util.Aeson

instance ToJSON PageMetadata where
  toJSON = genericToJSON jsonOptions

instance FromJSON PageMetadata where
  parseJSON = genericParseJSON jsonOptions
