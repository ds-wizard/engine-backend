module Registry.Api.Resource.Organization.OrganizationJM where

import Data.Aeson

import Registry.Api.Resource.Organization.Common ()
import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Util.JSON (simpleParseJSON, simpleToJSON)

instance ToJSON OrganizationDTO where
  toJSON = simpleToJSON "_organizationDTO"

instance FromJSON OrganizationDTO where
  parseJSON = simpleParseJSON "_organizationDTO"
