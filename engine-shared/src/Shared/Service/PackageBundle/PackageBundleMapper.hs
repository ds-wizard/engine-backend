module Shared.Service.PackageBundle.PackageBundleMapper where

import Control.Lens ((^.))

import LensesConfig
import Shared.Api.Resource.PackageBundle.PackageBundleDTO
import Shared.Model.PackageBundle.PackageBundle
import qualified Shared.Service.Package.PackageMapper as PM

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
