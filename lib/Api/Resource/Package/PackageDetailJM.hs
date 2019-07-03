module Api.Resource.Package.PackageDetailJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Organization.OrganizationSimpleJM ()
import Api.Resource.Package.Common
import Api.Resource.Package.PackageDetailDTO

instance ToJSON PackageDetailDTO where
  toJSON PackageDetailDTO {..} =
    object
      [ "id" .= _packageDetailDTOPId
      , "name" .= _packageDetailDTOName
      , "organizationId" .= _packageDetailDTOOrganizationId
      , "kmId" .= _packageDetailDTOKmId
      , "version" .= _packageDetailDTOVersion
      , "metamodelVersion" .= _packageDetailDTOMetamodelVersion
      , "description" .= _packageDetailDTODescription
      , "readme" .= _packageDetailDTOReadme
      , "license" .= _packageDetailDTOLicense
      , "parentPackageId" .= _packageDetailDTOParentPackageId
      , "versions" .= _packageDetailDTOVersions
      , "remoteLatestVersion" .= _packageDetailDTORemoteLatestVersion
      , "organization" .= _packageDetailDTOOrganization
      , "registryLink" .= _packageDetailDTORegistryLink
      , "state" .= serializePackageState _packageDetailDTOState
      , "createdAt" .= _packageDetailDTOCreatedAt
      ]

instance FromJSON PackageDetailDTO where
  parseJSON (Object o) = do
    _packageDetailDTOPId <- o .: "id"
    _packageDetailDTOName <- o .: "name"
    _packageDetailDTOOrganizationId <- o .: "organizationId"
    _packageDetailDTOKmId <- o .: "kmId"
    _packageDetailDTOVersion <- o .: "version"
    _packageDetailDTOMetamodelVersion <- o .: "metamodelVersion"
    _packageDetailDTODescription <- o .: "description"
    _packageDetailDTOReadme <- o .: "readme"
    _packageDetailDTOLicense <- o .: "license"
    _packageDetailDTOParentPackageId <- o .: "parentPackageId"
    _packageDetailDTOVersions <- o .: "versions"
    _packageDetailDTORemoteLatestVersion <- o .: "remoteLatestVersion"
    _packageDetailDTOOrganization <- o .: "organization"
    _packageDetailDTORegistryLink <- o .: "registryLink"
    _packageDetailDTOCreatedAt <- o .: "createdAt"
    hDeserializePackageState o $ \_packageDetailDTOState -> return PackageDetailDTO {..}
  parseJSON _ = mzero
