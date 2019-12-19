module Registry.Service.PackageBundle.PackageBundleMapper where

import Control.Lens ((^.))

import Registry.Api.Resource.PackageBundle.PackageBundleDTO
import Registry.LensesConfig
import qualified Registry.Service.Package.PackageMapper as PM
import Shared.Model.PackageBundle.PackageBundle

toDTO :: PackageBundle -> PackageBundleDTO
toDTO pb =
  PackageBundleDTO
    { _packageBundleDTOBundleId = pb ^. bundleId
    , _packageBundleDTOName = pb ^. name
    , _packageBundleDTOOrganizationId = pb ^. organizationId
    , _packageBundleDTOKmId = pb ^. kmId
    , _packageBundleDTOVersion = pb ^. version
    , _packageBundleDTOMetamodelVersion = pb ^. metamodelVersion
    , _packageBundleDTOPackages = PM.toDTO <$> pb ^. packages
    }
