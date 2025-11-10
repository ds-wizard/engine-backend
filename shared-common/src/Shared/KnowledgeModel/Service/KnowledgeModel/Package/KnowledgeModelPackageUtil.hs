module Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil where

import qualified Data.List as L

import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.List (groupBy)
import Shared.Coordinate.Service.Coordinate.CoordinateValidation
import Shared.Coordinate.Util.Coordinate
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern

groupPackages :: [KnowledgeModelPackage] -> [[KnowledgeModelPackage]]
groupPackages = groupBy (\p1 p2 -> p1.organizationId == p2.organizationId && p1.kmId == p2.kmId)

sortPackagesByVersion :: [KnowledgeModelPackage] -> [KnowledgeModelPackage]
sortPackagesByVersion = L.sortBy (\p1 p2 -> compareVersionNeg p1.version p2.version)

upgradePackageVersion :: String -> String -> String
upgradePackageVersion pkgId = buildCoordinate (getOrgIdFromCoordinate pkgId) (getKmIdFromCoordinate pkgId)

fitsIntoKMSpecs :: [String] -> [KnowledgeModelPackagePattern] -> Bool
fitsIntoKMSpecs pkgIdSplit = foldl (go pkgIdSplit) False
  where
    go :: [String] -> Bool -> KnowledgeModelPackagePattern -> Bool
    go pkgIdSplit acc packagePattern = acc || fitsIntoKMSpec pkgIdSplit packagePattern

fitsIntoKMSpec :: [String] -> KnowledgeModelPackagePattern -> Bool
fitsIntoKMSpec pkgIdSplit kmSpec = heCompareOrgId $ heCompareKmId $ heCompareVersionMin $ heCompareVersionMax True
  where
    heCompareOrgId callback =
      case kmSpec.orgId of
        Just orgId -> (head pkgIdSplit == orgId) && callback
        Nothing -> callback
    heCompareKmId callback =
      case kmSpec.kmId of
        Just kmId -> ((pkgIdSplit !! 1) == kmId) && callback
        Nothing -> callback
    heCompareVersionMin callback =
      case kmSpec.minVersion of
        Just minVersion ->
          case compareVersion (pkgIdSplit !! 2) minVersion of
            LT -> False
            _ -> callback
        Nothing -> callback
    heCompareVersionMax callback =
      case kmSpec.maxVersion of
        Just maxVersion ->
          case compareVersion (pkgIdSplit !! 2) maxVersion of
            GT -> False
            _ -> callback
        Nothing -> callback

resolvePackageId :: AppContextC s sc m => String -> m String
resolvePackageId pId = do
  validateCoordinateFormat True "kmId" pId
  let version = getVersionFromCoordinate pId
  if version == "latest"
    then do
      let orgId = getOrgIdFromCoordinate pId
      let kmId = getKmIdFromCoordinate pId
      versions <- findVersionsForPackage orgId kmId
      let latest = L.maximumBy compareVersion versions
      return $ buildCoordinate orgId kmId latest
    else return pId
