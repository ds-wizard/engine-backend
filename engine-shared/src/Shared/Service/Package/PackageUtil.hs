module Shared.Service.Package.PackageUtil where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader)
import qualified Data.List as L
import Data.Pool
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Records

import Shared.Database.DAO.Package.PackageDAO
import Shared.Model.Error.Error
import Shared.Model.Package.Package
import Shared.Model.Package.PackagePattern
import Shared.Service.Coordinate.CoordinateValidation
import Shared.Util.Coordinate
import Shared.Util.List (groupBy)

groupPackages :: [Package] -> [[Package]]
groupPackages = groupBy (\p1 p2 -> p1.organizationId == p2.organizationId && p1.kmId == p2.kmId)

sortPackagesByVersion :: [Package] -> [Package]
sortPackagesByVersion = L.sortBy (\p1 p2 -> compareVersionNeg p1.version p2.version)

upgradePackageVersion :: String -> String -> String
upgradePackageVersion pkgId = buildCoordinate (getOrgIdFromCoordinate pkgId) (getKmIdFromCoordinate pkgId)

fitsIntoKMSpecs :: [String] -> [PackagePattern] -> Bool
fitsIntoKMSpecs pkgIdSplit = foldl (go pkgIdSplit) False
  where
    go :: [String] -> Bool -> PackagePattern -> Bool
    go pkgIdSplit acc packagePattern = acc || fitsIntoKMSpec pkgIdSplit packagePattern

fitsIntoKMSpec :: [String] -> PackagePattern -> Bool
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

resolvePackageId
  :: ( MonadError AppError m
     , MonadLogger m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => String
  -> m String
resolvePackageId pId = do
  validateCoordinateFormat True pId
  let version = getVersionFromCoordinate pId
  if version == "latest"
    then do
      let orgId = getOrgIdFromCoordinate pId
      let kmId = getKmIdFromCoordinate pId
      versions <- findVersionsForPackage orgId kmId
      let latest = L.maximumBy compareVersion versions
      return $ buildCoordinate orgId kmId latest
    else return pId
