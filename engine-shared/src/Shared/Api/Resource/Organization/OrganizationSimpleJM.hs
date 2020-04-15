module Shared.Api.Resource.Organization.OrganizationSimpleJM where

import Data.Aeson

import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON OrganizationSimpleDTO where
  parseJSON = simpleParseJSON "_organizationSimpleDTO"

instance ToJSON OrganizationSimpleDTO where
  toJSON = simpleToJSON "_organizationSimpleDTO"
