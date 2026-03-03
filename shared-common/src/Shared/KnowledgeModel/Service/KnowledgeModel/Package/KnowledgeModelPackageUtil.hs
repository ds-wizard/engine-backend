module Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil where

import qualified Data.List as L

import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.List (groupBy)
import Shared.Coordinate.Model.Coordinate.Coordinate
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

fitsIntoKMSpecs :: Coordinate -> [KnowledgeModelPackagePattern] -> Bool
fitsIntoKMSpecs coordinate = foldl (go coordinate) False
  where
    go :: Coordinate -> Bool -> KnowledgeModelPackagePattern -> Bool
    go coordinate acc packagePattern = acc || fitsIntoKMSpec coordinate packagePattern

fitsIntoKMSpec :: Coordinate -> KnowledgeModelPackagePattern -> Bool
fitsIntoKMSpec coordinate kmSpec = heCompareOrgId $ heCompareKmId $ heCompareVersionMin $ heCompareVersionMax True
  where
    heCompareOrgId callback =
      case kmSpec.orgId of
        Just orgId -> (coordinate.organizationId == orgId) && callback
        Nothing -> callback
    heCompareKmId callback =
      case kmSpec.kmId of
        Just kmId -> (coordinate.entityId == kmId) && callback
        Nothing -> callback
    heCompareVersionMin callback =
      case kmSpec.minVersion of
        Just minVersion ->
          case compareVersion coordinate.version minVersion of
            LT -> False
            _ -> callback
        Nothing -> callback
    heCompareVersionMax callback =
      case kmSpec.maxVersion of
        Just maxVersion ->
          case compareVersion coordinate.version maxVersion of
            GT -> False
            _ -> callback
        Nothing -> callback

resolvePackageCoordinate :: AppContextC s sc m => Coordinate -> m KnowledgeModelPackage
resolvePackageCoordinate coordinate =
  if coordinate.version == "latest"
    then findLatestPackageByOrganizationIdAndKmId coordinate.organizationId coordinate.entityId Nothing
    else findPackageByCoordinate coordinate
