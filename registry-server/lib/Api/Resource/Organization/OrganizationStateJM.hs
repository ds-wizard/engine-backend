module Api.Resource.Organization.OrganizationStateJM where

import Data.Aeson

import Api.Resource.Organization.OrganizationStateDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance ToJSON OrganizationStateDTO where
  toJSON = simpleToJSON "_organizationStateDTO"

instance FromJSON OrganizationStateDTO where
  parseJSON = simpleParseJSON "_organizationStateDTO"
