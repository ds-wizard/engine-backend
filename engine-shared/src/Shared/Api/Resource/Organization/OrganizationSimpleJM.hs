module Shared.Api.Resource.Organization.OrganizationSimpleJM where

import Data.Aeson

import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Util.Aeson

instance FromJSON OrganizationSimpleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON OrganizationSimpleDTO where
  toJSON = genericToJSON jsonOptions
