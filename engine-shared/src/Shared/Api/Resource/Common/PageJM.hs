module Shared.Api.Resource.Common.PageJM where

import Data.Aeson
import Data.String (fromString)

import Shared.Api.Resource.Common.PageMetadataJM ()
import Shared.Model.Common.Page

instance ToJSON entity => ToJSON (Page entity) where
  toJSON (Page name page entities) = object ["page" .= page, "_embedded" .= object [fromString name .= entities]]
