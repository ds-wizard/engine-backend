module Api.Resource.Organization.OrganizationChangeJM where

import Data.Aeson

import Api.Resource.Organization.OrganizationChangeDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON OrganizationChangeDTO where
  parseJSON = simpleParseJSON "_organizationChangeDTO"

instance ToJSON OrganizationChangeDTO where
  toJSON = simpleToJSON "_organizationChangeDTO"
