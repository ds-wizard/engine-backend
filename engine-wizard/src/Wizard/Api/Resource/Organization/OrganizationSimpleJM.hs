module Wizard.Api.Resource.Organization.OrganizationSimpleJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Organization.OrganizationSimpleDTO

instance FromJSON OrganizationSimpleDTO where
  parseJSON = simpleParseJSON "_organizationSimpleDTO"

instance ToJSON OrganizationSimpleDTO where
  toJSON = simpleToJSON "_organizationSimpleDTO"
