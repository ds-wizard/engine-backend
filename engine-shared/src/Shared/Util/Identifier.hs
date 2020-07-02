module Shared.Util.Identifier where

import Control.Lens ((^.))
import qualified Data.List as L

import LensesConfig
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
    versionAMajor = read (head versionASplitted) :: Int
    versionAMinor = read (versionASplitted !! 1) :: Int
    versionAPatch = read (versionASplitted !! 2) :: Int
    versionBMajor = read (head versionBSplitted) :: Int
    versionBMinor = read (versionBSplitted !! 1) :: Int
    versionBPatch = read (versionBSplitted !! 2) :: Int

splitPackageId :: String -> [String]
splitPackageId = splitOn ":"

getOrgIdFromPkgId :: String -> String
getOrgIdFromPkgId pkgId = head $ splitPackageId pkgId

getKmIdFromPkgId :: String -> String
getKmIdFromPkgId pkgId = splitPackageId pkgId !! 1

getVersionFromPkgId :: String -> String
getVersionFromPkgId pkgId = splitPackageId pkgId !! 2

splitVersion :: String -> [String]
splitVersion = splitOn "."

buildIdentifierId :: String -> String -> String -> String
buildIdentifierId orgId entityId version = orgId ++ ":" ++ entityId ++ ":" ++ version

chooseTheNewest :: (HasVersion a String, Ord a) => [[a]] -> [a]
chooseTheNewest = fmap (L.maximumBy (\t1 t2 -> compare (t1 ^. version) (t2 ^. version)))
