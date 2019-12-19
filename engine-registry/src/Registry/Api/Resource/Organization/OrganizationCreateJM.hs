module Registry.Api.Resource.Organization.OrganizationCreateJM where

import Data.Aeson

import Registry.Api.Resource.Organization.OrganizationCreateDTO
import Registry.Util.JSON (simpleParseJSON, simpleToJSON)

instance ToJSON OrganizationCreateDTO where
  toJSON = simpleToJSON "_organizationCreateDTO"

instance FromJSON OrganizationCreateDTO where
  parseJSON = simpleParseJSON "_organizationCreateDTO"
