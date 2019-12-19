module Wizard.Api.Resource.Organization.OrganizationSimpleJM where

import Data.Aeson

import Wizard.Api.Resource.Organization.OrganizationSimpleDTO
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON OrganizationSimpleDTO where
  parseJSON = simpleParseJSON "_organizationSimpleDTO"

instance ToJSON OrganizationSimpleDTO where
  toJSON = simpleToJSON "_organizationSimpleDTO"
