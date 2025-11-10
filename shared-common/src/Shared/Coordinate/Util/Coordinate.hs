module Shared.Coordinate.Util.Coordinate where

import qualified Data.List as L
import GHC.Records

import Shared.Common.Util.String (splitOn)

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
    versionASplit = splitVersion versionA
    versionBSplit = splitVersion versionB
    versionAMajor = read (head versionASplit) :: Int
    versionAMinor = read (versionASplit !! 1) :: Int
    versionAPatch = read (versionASplit !! 2) :: Int
    versionBMajor = read (head versionBSplit) :: Int
    versionBMinor = read (versionBSplit !! 1) :: Int
    versionBPatch = read (versionBSplit !! 2) :: Int

splitCoordinate :: String -> [String]
splitCoordinate = splitOn ":"

getOrgIdFromCoordinate :: String -> String
getOrgIdFromCoordinate coordinate = head $ splitCoordinate coordinate

getKmIdFromCoordinate :: String -> String
getKmIdFromCoordinate coordinate = splitCoordinate coordinate !! 1

getTemplateIdFromCoordinate :: String -> String
getTemplateIdFromCoordinate coordinate = splitCoordinate coordinate !! 1

getVersionFromCoordinate :: String -> String
getVersionFromCoordinate coordinate = splitCoordinate coordinate !! 2

splitVersion :: String -> [String]
splitVersion = splitOn "."

buildCoordinate :: String -> String -> String -> String
buildCoordinate orgId entityId version = orgId ++ ":" ++ entityId ++ ":" ++ version

chooseTheNewest :: (HasField "version" a String, Ord a) => [[a]] -> [a]
chooseTheNewest = fmap (L.maximumBy (\t1 t2 -> compareVersion t1.version t2.version))
