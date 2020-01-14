module Registry.Api.Resource.Organization.OrganizationStateJM where

import Data.Aeson

import Registry.Api.Resource.Organization.OrganizationStateDTO
import Registry.Util.JSON (simpleParseJSON, simpleToJSON)

instance ToJSON OrganizationStateDTO where
  toJSON = simpleToJSON "_organizationStateDTO"

instance FromJSON OrganizationStateDTO where
  parseJSON = simpleParseJSON "_organizationStateDTO"
