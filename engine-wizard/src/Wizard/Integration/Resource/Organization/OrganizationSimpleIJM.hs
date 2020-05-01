module Wizard.Integration.Resource.Organization.OrganizationSimpleIJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Integration.Resource.Organization.OrganizationSimpleIDTO

instance FromJSON OrganizationSimpleIDTO where
  parseJSON = simpleParseJSON "_organizationSimpleIDTO"

instance ToJSON OrganizationSimpleIDTO where
  toJSON OrganizationSimpleIDTO {..} =
    object
      [ "organizationId" .= _organizationSimpleIDTOOrganizationId
      , "name" .= _organizationSimpleIDTOName
      , "logo" .= _organizationSimpleIDTOLogo
      ]
