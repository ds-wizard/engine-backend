module Api.Resource.Organization.OrganizationChangeDTO where

import Control.Monad
import Data.Aeson
import Data.UUID
import GHC.Generics

data OrganizationChangeDTO = OrganizationChangeDTO
  { _organizationChangeDTOUuid :: UUID
  , _organizationChangeDTOName :: String
  , _organizationChangeDTOOrganizationId :: String
  } deriving (Show, Eq, Generic)

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
