module Registry.Service.PackageBundle.PackageBundleMapper where

import Control.Lens ((^.))

import LensesConfig
import Registry.Api.Resource.PackageBundle.PackageBundleDTO
import Registry.Model.PackageBundle.PackageBundle
import qualified Registry.Service.Package.PackageMapper as PM

toDTO :: PackageBundle -> PackageBundleDTO
toDTO pb =
  PackageBundleDTO
    { _packageBundleDTOBundleId = pb ^. bundleId
    , _packageBundleDTOName = pb ^. name
    , _packageBundleDTOOrganizationId = pb ^. organizationId
    , _packageBundleDTOKmId = pb ^. kmId
    , _packageBundleDTOVersion = pb ^. version
    , _packageBundleDTOMetamodelVersion = pb ^. metamodelVersion
    , _packageBundleDTOPackages = PM.toRawDTO <$> pb ^. packages
    }
