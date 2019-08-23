module Service.Package.PackageUtils where

import Control.Lens ((^.))
import Data.List

import Integration.Resource.Package.PackageSimpleIDTO
import LensesConfig
import Model.Package.Package
import Model.Package.PackageState
import Util.String (splitOn)

compareVersionNeg :: String -> String -> Ordering
compareVersionNeg verA verB = compareVersion verB verA

compareVersion :: String -> String -> Ordering
compareVersion versionA versionB =
  case compare versionAMajor versionBMajor of
    LT -> LT
    GT -> GT
    EQ ->
      case compare versionAMinor versionBMinor of
        LT -> LT
        GT -> GT
        EQ ->
          case compare versionAPatch versionBPatch of
            LT -> LT
            GT -> GT
            EQ -> EQ
  where
    versionASplitted = splitVersion versionA
    versionBSplitted = splitVersion versionB
    versionAMajor = read (versionASplitted !! 0) :: Int
    versionAMinor = read (versionASplitted !! 1) :: Int
    versionAPatch = read (versionASplitted !! 2) :: Int
    versionBMajor = read (versionBSplitted !! 0) :: Int
    versionBMinor = read (versionBSplitted !! 1) :: Int
    versionBPatch = read (versionBSplitted !! 2) :: Int

sortPackagesByVersion :: [Package] -> [Package]
sortPackagesByVersion = sortBy (\p1 p2 -> compareVersionNeg (p1 ^. version) (p2 ^. version))

splitPackageId :: String -> [String]
splitPackageId packageId = splitOn ":" packageId

getOrgIdFromPkgId :: String -> String
getOrgIdFromPkgId pkgId = splitPackageId pkgId !! 0

getKmIdFromPkgId :: String -> String
getKmIdFromPkgId pkgId = splitPackageId pkgId !! 1

getVersionFromPkgId :: String -> String
getVersionFromPkgId pkgId = splitPackageId pkgId !! 2

splitVersion :: String -> [String]
splitVersion pkgVersion = splitOn "." pkgVersion

buildPackageId :: String -> String -> String -> String
buildPackageId pkgOrganizationId pkgKmId pkgVersion = pkgOrganizationId ++ ":" ++ pkgKmId ++ ":" ++ pkgVersion

upgradePackageVersion :: String -> String -> String
upgradePackageVersion pkgId newVersion = buildPackageId (getOrgIdFromPkgId pkgId) (getKmIdFromPkgId pkgId) newVersion

selectPackageByOrgIdAndKmId pkg =
  find (\p -> (p ^. organizationId) == (pkg ^. organizationId) && (p ^. kmId) == (pkg ^. kmId))

computePackageState :: [PackageSimpleIDTO] -> Package -> PackageState
computePackageState pkgsFromRegistry pkg =
  case selectPackageByOrgIdAndKmId pkg pkgsFromRegistry of
    Just pkgFromRegistry ->
      case compareVersion (pkgFromRegistry ^. version) (pkg ^. version) of
        LT -> UnpublishedPackageState
        EQ -> UpToDatePackageState
        GT -> OutdatedPackageState
    Nothing -> UnknownPackageState
