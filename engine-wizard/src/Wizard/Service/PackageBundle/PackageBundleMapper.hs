module Wizard.Service.PackageBundle.PackageBundleMapper where

import Control.Lens ((^.))

import Shared.Model.PackageBundle.PackageBundle
import Wizard.Api.Resource.PackageBundle.PackageBundleDTO
import Wizard.LensesConfig
import qualified Wizard.Service.Package.PackageMapper as PM

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
