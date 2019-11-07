module Api.Resource.Organization.OrganizationSimpleJM where

import Data.Aeson

import Api.Resource.Organization.OrganizationSimpleDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance ToJSON OrganizationSimpleDTO where
  toJSON = simpleToJSON "_organizationSimpleDTO"

instance FromJSON OrganizationSimpleDTO where
  parseJSON = simpleParseJSON "_organizationSimpleDTO"
