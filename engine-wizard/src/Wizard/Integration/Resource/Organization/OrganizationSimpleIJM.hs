module Wizard.Integration.Resource.Organization.OrganizationSimpleIJM where

import Data.Aeson

import Wizard.Integration.Resource.Organization.OrganizationSimpleIDTO
import Wizard.Util.JSON (simpleParseJSON)

instance FromJSON OrganizationSimpleIDTO where
  parseJSON = simpleParseJSON "_organizationSimpleIDTO"

instance ToJSON OrganizationSimpleIDTO where
  toJSON OrganizationSimpleIDTO {..} =
    object
      [ "organizationId" .= _organizationSimpleIDTOOrganizationId
      , "name" .= _organizationSimpleIDTOName
      , "logo" .= _organizationSimpleIDTOLogo
      ]
