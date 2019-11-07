module Api.Resource.Organization.OrganizationCreateJM where

import Data.Aeson

import Api.Resource.Organization.OrganizationCreateDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance ToJSON OrganizationCreateDTO where
  toJSON = simpleToJSON "_organizationCreateDTO"

instance FromJSON OrganizationCreateDTO where
  parseJSON = simpleParseJSON "_organizationCreateDTO"
