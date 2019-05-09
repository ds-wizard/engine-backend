module Api.Resource.Organization.OrganizationChangeJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Organization.OrganizationChangeDTO

instance FromJSON OrganizationChangeDTO where
  parseJSON (Object o) = do
    _organizationChangeDTOUuid <- o .: "uuid"
    _organizationChangeDTOName <- o .: "name"
    _organizationChangeDTOOrganizationId <- o .: "organizationId"
    return OrganizationChangeDTO {..}
  parseJSON _ = mzero

instance ToJSON OrganizationChangeDTO where
  toJSON OrganizationChangeDTO {..} =
    object
      [ "uuid" .= _organizationChangeDTOUuid
      , "name" .= _organizationChangeDTOName
      , "organizationId" .= _organizationChangeDTOOrganizationId
      ]
