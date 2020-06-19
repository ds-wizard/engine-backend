module Wizard.Service.Package.PackageUtils where

import Control.Lens ((^.))
import Data.List

import LensesConfig
import Registry.Api.Resource.Package.PackageSimpleDTO
import Shared.Model.Package.Package
import Wizard.Model.Package.PackageState
import Wizard.Util.IdentifierUtil

sortPackagesByVersion :: [Package] -> [Package]
sortPackagesByVersion = sortBy (\p1 p2 -> compareVersionNeg (p1 ^. version) (p2 ^. version))

upgradePackageVersion :: String -> String -> String
upgradePackageVersion pkgId = buildPackageId (getOrgIdFromPkgId pkgId) (getKmIdFromPkgId pkgId)

selectPackageByOrgIdAndKmId pkg =
  find (\p -> (p ^. organizationId) == (pkg ^. organizationId) && (p ^. kmId) == (pkg ^. kmId))

selectOrganizationByOrgId pkg = find (\org -> (org ^. organizationId) == (pkg ^. organizationId))

computePackageState :: [PackageSimpleDTO] -> Package -> PackageState
computePackageState pkgsFromRegistry pkg =
  case selectPackageByOrgIdAndKmId pkg pkgsFromRegistry of
    Just pkgFromRegistry ->
      case compareVersion (pkgFromRegistry ^. version) (pkg ^. version) of
        LT -> UnpublishedPackageState
        EQ -> UpToDatePackageState
        GT -> OutdatedPackageState
    Nothing -> UnknownPackageState

getNewestUniquePackages :: [Package] -> [Package]
getNewestUniquePackages = fmap chooseNewest . groupPackages
  where
    groupPackages :: [Package] -> [[Package]]
    groupPackages = groupBy (\p1 p2 -> (p1 ^. organizationId) == (p2 ^. organizationId) && (p1 ^. kmId) == (p2 ^. kmId))
    chooseNewest :: [Package] -> Package
    chooseNewest = maximumBy (\p1 p2 -> compareVersion (p1 ^. version) (p2 ^. version))
