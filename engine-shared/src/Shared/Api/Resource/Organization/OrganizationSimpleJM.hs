module Shared.Api.Resource.Organization.OrganizationSimpleJM where

import Data.Aeson

import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Util.JSON

instance FromJSON OrganizationSimpleDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON OrganizationSimpleDTO where
  toJSON = genericToJSON simpleOptions
