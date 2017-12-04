module Api.Resources.Organization.OrganizationDTO where

import Control.Lens ((^.), makeLenses)
import Control.Monad
import Data.Aeson
import Data.Text
import Data.UUID
import GHC.Generics

import Common.Types
import Common.Uuid

data OrganizationDTO = OrganizationDTO
  { _orgdtoUuid :: UUID
  , _orgdtoName :: String
  , _orgdtoGroupId :: String
  } deriving (Show, Eq, Generic)

makeLenses ''OrganizationDTO

instance FromJSON OrganizationDTO where
  parseJSON (Object o) = do
    _orgdtoUuid <- o .: "uuid"
    _orgdtoName <- o .: "name"
    _orgdtoGroupId <- o .: "groupId"
    return OrganizationDTO {..}
  parseJSON _ = mzero

instance ToJSON OrganizationDTO where
  toJSON OrganizationDTO {..} = object ["uuid" .= _orgdtoUuid, "name" .= _orgdtoName, "groupId" .= _orgdtoGroupId]
