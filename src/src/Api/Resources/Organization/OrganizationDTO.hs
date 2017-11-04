module Api.Resources.Organization.OrganizationDTO where

import Control.Lens (makeLenses, (^.))
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
  , _orgdtoNamespace :: String
  } deriving (Show, Eq, Generic)

makeLenses ''OrganizationDTO

instance FromJSON OrganizationDTO where
  parseJSON (Object o) = do
    _orgdtoUuid <- o .: "organizationUuid"
    _orgdtoName <- o .: "name"
    _orgdtoNamespace <- o .: "namespace"
    return OrganizationDTO {..}
  parseJSON _ = mzero

instance ToJSON OrganizationDTO where
  toJSON OrganizationDTO {..} =
    object
      [ "organizationUuid" .= _orgdtoUuid
      , "name" .= _orgdtoName
      , "namespace" .= _orgdtoNamespace
      ]
