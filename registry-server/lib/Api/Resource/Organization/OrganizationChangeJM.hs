module Api.Resource.Organization.OrganizationChangeJM where

import Data.Aeson

import Api.Resource.Organization.OrganizationChangeDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance ToJSON OrganizationChangeDTO where
  toJSON = simpleToJSON "_organizationChangeDTO"

instance FromJSON OrganizationChangeDTO where
  parseJSON = simpleParseJSON "_organizationChangeDTO"
