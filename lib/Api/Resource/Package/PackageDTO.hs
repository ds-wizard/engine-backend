module Api.Resource.Package.PackageDTO where

import Control.Monad
import Data.Aeson

data PackageDTO = PackageDTO
  { _packageDTOPId :: String
  , _packageDTOName :: String
  , _packageDTOOrganizationId :: String
  , _packageDTOKmId :: String
  , _packageDTOVersion :: String
  , _packageDTODescription :: String
  , _packageDTOParentPackageId :: Maybe String
  } deriving (Show, Eq)

instance ToJSON PackageDTO where
  toJSON PackageDTO {..} =
    object
      [ "id" .= _packageDTOPId
      , "name" .= _packageDTOName
      , "organizationId" .= _packageDTOOrganizationId
      , "kmId" .= _packageDTOKmId
      , "version" .= _packageDTOVersion
      , "description" .= _packageDTODescription
      , "parentPackageId" .= _packageDTOParentPackageId
      ]

instance FromJSON PackageDTO where
  parseJSON (Object o) = do
    _packageDTOPId <- o .: "id"
    _packageDTOName <- o .: "name"
    _packageDTOOrganizationId <- o .: "organizationId"
    _packageDTOKmId <- o .: "kmId"
    _packageDTOVersion <- o .: "version"
    _packageDTODescription <- o .: "description"
    _packageDTOParentPackageId <- o .: "parentPackageId"
    return PackageDTO {..}
  parseJSON _ = mzero
