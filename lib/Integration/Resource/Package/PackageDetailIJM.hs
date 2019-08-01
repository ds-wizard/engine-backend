module Integration.Resource.Package.PackageDetailIJM where

import Control.Monad
import Data.Aeson

import Integration.Resource.Organization.OrganizationSimpleIJM ()
import Integration.Resource.Package.PackageDetailIDTO

instance ToJSON PackageDetailIDTO where
  toJSON PackageDetailIDTO {..} =
    object
      [ "id" .= _packageDetailIDTOPId
      , "name" .= _packageDetailIDTOName
      , "organizationId" .= _packageDetailIDTOOrganizationId
      , "kmId" .= _packageDetailIDTOKmId
      , "version" .= _packageDetailIDTOVersion
      , "metamodelVersion" .= _packageDetailIDTOMetamodelVersion
      , "description" .= _packageDetailIDTODescription
      , "readme" .= _packageDetailIDTOReadme
      , "license" .= _packageDetailIDTOLicense
      , "previousPackageId" .= _packageDetailIDTOPreviousPackageId
      , "forkOfPackageId" .= _packageDetailIDTOForkOfPackageId
      , "mergeCheckpointPackageId" .= _packageDetailIDTOMergeCheckpointPackageId
      , "versions" .= _packageDetailIDTOVersions
      , "organization" .= _packageDetailIDTOOrganization
      , "createdAt" .= _packageDetailIDTOCreatedAt
      ]

instance FromJSON PackageDetailIDTO where
  parseJSON (Object o) = do
    _packageDetailIDTOPId <- o .: "id"
    _packageDetailIDTOName <- o .: "name"
    _packageDetailIDTOOrganizationId <- o .: "organizationId"
    _packageDetailIDTOKmId <- o .: "kmId"
    _packageDetailIDTOVersion <- o .: "version"
    _packageDetailIDTOMetamodelVersion <- o .: "metamodelVersion"
    _packageDetailIDTODescription <- o .: "description"
    _packageDetailIDTOReadme <- o .: "readme"
    _packageDetailIDTOLicense <- o .: "license"
    _packageDetailIDTOPreviousPackageId <- o .: "previousPackageId"
    _packageDetailIDTOForkOfPackageId <- o .: "forkOfPackageId"
    _packageDetailIDTOMergeCheckpointPackageId <- o .: "mergeCheckpointPackageId"
    _packageDetailIDTOVersions <- o .: "versions"
    _packageDetailIDTOOrganization <- o .: "organization"
    _packageDetailIDTOCreatedAt <- o .: "createdAt"
    return PackageDetailIDTO {..}
  parseJSON _ = mzero
