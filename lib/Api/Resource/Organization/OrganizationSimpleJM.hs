module Api.Resource.Organization.OrganizationSimpleJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Organization.OrganizationSimpleDTO

instance FromJSON OrganizationSimpleDTO where
  parseJSON (Object o) = do
    _organizationSimpleDTOName <- o .: "name"
    _organizationSimpleDTOOrganizationId <- o .: "organizationId"
    _organizationSimpleDTOLogo <- o .: "logo"
    return OrganizationSimpleDTO {..}
  parseJSON _ = mzero

instance ToJSON OrganizationSimpleDTO where
  toJSON OrganizationSimpleDTO {..} =
    object
      [ "name" .= _organizationSimpleDTOName
      , "organizationId" .= _organizationSimpleDTOOrganizationId
      , "logo" .= _organizationSimpleDTOLogo
      ]
