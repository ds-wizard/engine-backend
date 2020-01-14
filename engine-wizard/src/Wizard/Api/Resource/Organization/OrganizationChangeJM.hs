module Wizard.Api.Resource.Organization.OrganizationChangeJM where

import Data.Aeson

import Wizard.Api.Resource.Organization.OrganizationChangeDTO
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON OrganizationChangeDTO where
  parseJSON = simpleParseJSON "_organizationChangeDTO"

instance ToJSON OrganizationChangeDTO where
  toJSON = simpleToJSON "_organizationChangeDTO"
