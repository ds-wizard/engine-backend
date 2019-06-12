module Integration.Resource.Package.PackageSimpleIJM where

import Control.Monad
import Data.Aeson

import Integration.Resource.Organization.OrganizationSimpleIJM ()
import Integration.Resource.Package.PackageSimpleIDTO

instance ToJSON PackageSimpleIDTO where
  toJSON PackageSimpleIDTO {..} =
    object
      [ "id" .= _packageSimpleIDTOPId
      , "name" .= _packageSimpleIDTOName
      , "organizationId" .= _packageSimpleIDTOOrganizationId
      , "kmId" .= _packageSimpleIDTOKmId
      , "version" .= _packageSimpleIDTOVersion
      , "description" .= _packageSimpleIDTODescription
      , "organization" .= _packageSimpleIDTOOrganization
      , "createdAt" .= _packageSimpleIDTOCreatedAt
      ]

instance FromJSON PackageSimpleIDTO where
  parseJSON (Object o) = do
    _packageSimpleIDTOPId <- o .: "id"
    _packageSimpleIDTOName <- o .: "name"
    _packageSimpleIDTOOrganizationId <- o .: "organizationId"
    _packageSimpleIDTOKmId <- o .: "kmId"
    _packageSimpleIDTOVersion <- o .: "version"
    _packageSimpleIDTODescription <- o .: "description"
    _packageSimpleIDTOOrganization <- o .: "organization"
    _packageSimpleIDTOCreatedAt <- o .: "createdAt"
    return PackageSimpleIDTO {..}
  parseJSON _ = mzero
