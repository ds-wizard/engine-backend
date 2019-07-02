module Api.Resource.Package.PackageSimpleJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Organization.OrganizationSimpleJM ()
import Api.Resource.Package.Common
import Api.Resource.Package.PackageSimpleDTO

instance ToJSON PackageSimpleDTO where
  toJSON PackageSimpleDTO {..} =
    object
      [ "id" .= _packageSimpleDTOPId
      , "name" .= _packageSimpleDTOName
      , "organizationId" .= _packageSimpleDTOOrganizationId
      , "kmId" .= _packageSimpleDTOKmId
      , "version" .= _packageSimpleDTOVersion
      , "versions" .= _packageSimpleDTOVersions
      , "description" .= _packageSimpleDTODescription
      , "organization" .= _packageSimpleDTOOrganization
      , "state" .= serializePackageState _packageSimpleDTOState
      , "createdAt" .= _packageSimpleDTOCreatedAt
      ]

instance FromJSON PackageSimpleDTO where
  parseJSON (Object o) = do
    _packageSimpleDTOPId <- o .: "id"
    _packageSimpleDTOName <- o .: "name"
    _packageSimpleDTOOrganizationId <- o .: "organizationId"
    _packageSimpleDTOKmId <- o .: "kmId"
    _packageSimpleDTOVersion <- o .: "version"
    _packageSimpleDTOVersions <- o .: "versions"
    _packageSimpleDTODescription <- o .: "description"
    _packageSimpleDTOOrganization <- o .: "organization"
    _packageSimpleDTOCreatedAt <- o .: "createdAt"
    hDeserializePackageState o $ \_packageSimpleDTOState -> return PackageSimpleDTO {..}
  parseJSON _ = mzero
