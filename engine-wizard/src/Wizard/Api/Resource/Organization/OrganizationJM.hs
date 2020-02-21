module Wizard.Api.Resource.Organization.OrganizationJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Organization.OrganizationDTO

instance FromJSON OrganizationDTO where
  parseJSON = simpleParseJSON "_organizationDTO"

instance ToJSON OrganizationDTO where
  toJSON = simpleToJSON "_organizationDTO"
