module Api.Resource.Organization.OrganizationJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Organization.OrganizationDTO

instance FromJSON OrganizationDTO where
  parseJSON (Object o) = do
    _organizationDTOUuid <- o .: "uuid"
    _organizationDTOName <- o .: "name"
    _organizationDTOOrganizationId <- o .: "organizationId"
    _organizationDTOCreatedAt <- o .: "createdAt"
    _organizationDTOUpdatedAt <- o .: "updatedAt"
    return OrganizationDTO {..}
  parseJSON _ = mzero

instance ToJSON OrganizationDTO where
  toJSON OrganizationDTO {..} =
    object
      [ "uuid" .= _organizationDTOUuid
      , "name" .= _organizationDTOName
      , "organizationId" .= _organizationDTOOrganizationId
      , "createdAt" .= _organizationDTOCreatedAt
      , "updatedAt" .= _organizationDTOUpdatedAt
      ]
