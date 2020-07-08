module Shared.Api.Resource.Common.PageMetadataJM where

import Data.Aeson

import Shared.Model.Common.PageMetadata
import Shared.Util.JSON

instance ToJSON PageMetadata where
  toJSON = simpleToJSON "_pageMetadata"
