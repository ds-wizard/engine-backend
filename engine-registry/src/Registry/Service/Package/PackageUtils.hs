module Registry.Service.Package.PackageUtils where

import Control.Lens ((^.))
import Data.List

import LensesConfig
import Shared.Model.Package.Package
import Shared.Util.String (splitOn)

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

getOrganizationIdFromPackageId :: String -> String
getOrganizationIdFromPackageId pkgId = splitPackageId pkgId !! 0

getKmIdFromPackageId :: String -> String
getKmIdFromPackageId pkgId = splitPackageId pkgId !! 1

getVersionFromPackageId :: String -> String
getVersionFromPackageId pkgId = splitPackageId pkgId !! 2

splitVersion :: String -> [String]
splitVersion pkgVersion = splitOn "." pkgVersion
