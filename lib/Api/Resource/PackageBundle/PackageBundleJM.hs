module Api.Resource.PackageBundle.PackageBundleJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Package.PackageWithEventsDTO ()
import Api.Resource.PackageBundle.PackageBundleDTO

instance FromJSON PackageBundleDTO where
  parseJSON (Object o) = do
    _packageBundleDTOBundleId <- o .: "id"
    _packageBundleDTOName <- o .: "name"
    _packageBundleDTOOrganizationId <- o .: "organizationId"
    _packageBundleDTOKmId <- o .: "kmId"
    _packageBundleDTOVersion <- o .: "version"
    _packageBundleDTOMetamodelVersion <- o .: "metamodelVersion"
    packagesSerialized <- o .: "packages"
    _packageBundleDTOPackages <- parseJSON packagesSerialized
    return PackageBundleDTO {..}
  parseJSON _ = mzero

instance ToJSON PackageBundleDTO where
  toJSON PackageBundleDTO {..} =
    object
      [ "id" .= _packageBundleDTOBundleId
      , "name" .= _packageBundleDTOName
      , "organizationId" .= _packageBundleDTOOrganizationId
      , "kmId" .= _packageBundleDTOKmId
      , "version" .= _packageBundleDTOVersion
      , "metamodelVersion" .= _packageBundleDTOMetamodelVersion
      , "packages" .= toJSON _packageBundleDTOPackages
      ]
