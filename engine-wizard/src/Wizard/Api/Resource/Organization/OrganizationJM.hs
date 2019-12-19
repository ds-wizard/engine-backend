module Wizard.Api.Resource.Organization.OrganizationJM where

import Data.Aeson

import Wizard.Api.Resource.Organization.OrganizationDTO
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON OrganizationDTO where
  parseJSON = simpleParseJSON "_organizationDTO"

instance ToJSON OrganizationDTO where
  toJSON = simpleToJSON "_organizationDTO"
