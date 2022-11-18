module Shared.Api.Resource.Common.PageMetadataJM where

import Data.Aeson

import Shared.Model.Common.PageMetadata
import Shared.Util.Aeson

instance ToJSON PageMetadata where
  toJSON = genericToJSON jsonOptions

instance FromJSON PageMetadata where
  parseJSON = genericParseJSON jsonOptions
