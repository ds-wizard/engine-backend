module Wizard.Api.Resource.Organization.OrganizationChangeJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Organization.OrganizationChangeDTO

instance FromJSON OrganizationChangeDTO where
  parseJSON = simpleParseJSON "_organizationChangeDTO"

instance ToJSON OrganizationChangeDTO where
  toJSON = simpleToJSON "_organizationChangeDTO"
