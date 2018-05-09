module Api.Resource.Organization.OrganizationDTO where

import Control.Monad
import Data.Aeson
import Data.Text
import Data.Time
import Data.UUID
import GHC.Generics

import Common.Types
import Common.Uuid

data OrganizationDTO = OrganizationDTO
  { _organizationDTOUuid :: UUID
  , _organizationDTOName :: String
  , _organizationDTOOrganizationId :: String
  , _organizationDTOCreatedAt :: UTCTime
  , _organizationDTOUpdatedAt :: UTCTime
  } deriving (Show, Generic)

instance Eq OrganizationDTO where
  a == b =
    _organizationDTOUuid a == _organizationDTOUuid b &&
    _organizationDTOName a == _organizationDTOName b &&
    _organizationDTOOrganizationId a == _organizationDTOOrganizationId b

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
