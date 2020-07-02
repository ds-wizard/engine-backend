module Wizard.Service.Package.PackageUtil where

import Control.Lens ((^.))
import qualified Data.List as L

import LensesConfig
import Registry.Api.Resource.Package.PackageSimpleDTO
import Shared.Model.Package.Package
import Shared.Util.Identifier
import Wizard.Model.Package.PackageState

selectPackageByOrgIdAndKmId pkg =
  L.find (\p -> (p ^. organizationId) == (pkg ^. organizationId) && (p ^. kmId) == (pkg ^. kmId))

selectOrganizationByOrgId pkg = L.find (\org -> (org ^. organizationId) == (pkg ^. organizationId))

computePackageState :: [PackageSimpleDTO] -> Package -> PackageState
computePackageState pkgsFromRegistry pkg =
  case selectPackageByOrgIdAndKmId pkg pkgsFromRegistry of
    Just pkgFromRegistry ->
      case compareVersion (pkgFromRegistry ^. version) (pkg ^. version) of
        LT -> UnpublishedPackageState
        EQ -> UpToDatePackageState
        GT -> OutdatedPackageState
    Nothing -> UnknownPackageState
