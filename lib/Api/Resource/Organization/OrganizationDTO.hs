module Api.Resource.Organization.OrganizationDTO where

import Control.Monad
import Data.Aeson
import Data.Text
import Data.UUID
import GHC.Generics

import Common.Types
import Common.Uuid

data OrganizationDTO = OrganizationDTO
  { _organizationDTOUuid :: UUID
  , _organizationDTOName :: String
  , _organizationDTOOrganizationId :: String
  } deriving (Show, Eq, Generic)

instance FromJSON OrganizationDTO where
  parseJSON (Object o) = do
    _organizationDTOUuid <- o .: "uuid"
    _organizationDTOName <- o .: "name"
    _organizationDTOOrganizationId <- o .: "organizationId"
    return OrganizationDTO {..}
  parseJSON _ = mzero

instance ToJSON OrganizationDTO where
  toJSON OrganizationDTO {..} =
    object
      [ "uuid" .= _organizationDTOUuid
      , "name" .= _organizationDTOName
      , "organizationId" .= _organizationDTOOrganizationId
      ]
