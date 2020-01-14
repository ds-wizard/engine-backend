module Registry.Api.Resource.Organization.OrganizationChangeJM where

import Data.Aeson

import Registry.Api.Resource.Organization.OrganizationChangeDTO
import Registry.Util.JSON (simpleParseJSON, simpleToJSON)

instance ToJSON OrganizationChangeDTO where
  toJSON = simpleToJSON "_organizationChangeDTO"

instance FromJSON OrganizationChangeDTO where
  parseJSON = simpleParseJSON "_organizationChangeDTO"
