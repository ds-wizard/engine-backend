module Integration.Resource.Organization.OrganizationSimpleIJM where

import Data.Aeson

import Integration.Resource.Organization.OrganizationSimpleIDTO
import Util.JSON (simpleParseJSON)

instance FromJSON OrganizationSimpleIDTO where
  parseJSON = simpleParseJSON "_organizationSimpleIDTO"

instance ToJSON OrganizationSimpleIDTO where
  toJSON OrganizationSimpleIDTO {..} =
    object
      [ "organizationId" .= _organizationSimpleIDTOOrganizationId
      , "name" .= _organizationSimpleIDTOName
      , "logo" .= _organizationSimpleIDTOLogo
      ]
