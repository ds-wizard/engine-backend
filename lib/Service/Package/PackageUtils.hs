module Service.Package.PackageUtils where

import Control.Lens ((^.))
import Data.List
import Data.Text (Text)
import qualified Data.Text as T

import LensesConfig
import Model.Package.Package

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
    versionAMajor = read . T.unpack $ (versionASplitted !! 0) :: Int
    versionAMinor = read . T.unpack $ (versionASplitted !! 1) :: Int
    versionAPatch = read . T.unpack $ (versionASplitted !! 2) :: Int
    versionBMajor = read . T.unpack $ (versionBSplitted !! 0) :: Int
    versionBMinor = read . T.unpack $ (versionBSplitted !! 1) :: Int
    versionBPatch = read . T.unpack $ (versionBSplitted !! 2) :: Int

sortPackagesByVersion :: [Package] -> [Package]
sortPackagesByVersion = sortBy (\p1 p2 -> compareVersionNeg (p1 ^. version) (p2 ^. version))

splitPackageId :: String -> [Text]
splitPackageId packageId = T.splitOn ":" (T.pack packageId)

splitVersion :: String -> [Text]
splitVersion pkgVersion = T.splitOn "." (T.pack pkgVersion)
