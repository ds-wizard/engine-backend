module Registry.Api.Resource.Organization.OrganizationSimpleJM where

import Data.Aeson

import Registry.Api.Resource.Organization.OrganizationSimpleDTO
import Registry.Util.JSON (simpleParseJSON, simpleToJSON)

instance ToJSON OrganizationSimpleDTO where
  toJSON = simpleToJSON "_organizationSimpleDTO"

instance FromJSON OrganizationSimpleDTO where
  parseJSON = simpleParseJSON "_organizationSimpleDTO"
