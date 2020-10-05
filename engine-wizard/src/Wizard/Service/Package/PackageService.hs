module Wizard.Service.Package.PackageService
  ( getSimplePackagesFiltered
  , getPackagesPage
  , getPackageById
  , getPackageDetailById
  , getSeriesOfPackages
  , getAllPreviousEventsSincePackageId
  , getAllPreviousEventsSincePackageIdAndUntilPackageId
  , getTheNewestPackageByOrganizationIdAndKmId
  , getNewerPackages
  , createPackage
  , deletePackagesByQueryParams
  , deletePackage
  ) where

import Control.Lens ((^.), (^..))
import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import Data.List (maximumBy)

import LensesConfig
import qualified Registry.Api.Resource.Package.PackageSimpleDTO as R_PackageSimpleDTO
import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Database.DAO.Package.PackageDAO
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Event.Event
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import Shared.Service.Package.PackageMapper
import Shared.Service.Package.PackageUtil
import Shared.Util.Identifier
import Wizard.Api.Resource.Package.PackageDetailDTO
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Integration.Http.Registry.Runner
import Wizard.Model.Context.AppContext
import Wizard.Service.Cache.PackageCache
import Wizard.Service.Common.ACL
import Wizard.Service.Package.PackageMapper
import Wizard.Service.Package.PackageValidation
import Wizard.Service.Statistics.StatisticsService
import Wizard.Util.Cache

getSimplePackagesFiltered :: [(String, String)] -> AppContextM [PackageSimpleDTO]
getSimplePackagesFiltered queryParams = do
  checkPermission _PM_READ_PERM
  pkgs <- findPackagesFiltered queryParams
  iStat <- getInstanceStatistics
  pkgRs <- retrievePackages iStat
  orgRs <- retrieveOrganizations
  return . fmap (toSimpleDTOs pkgRs orgRs) . groupPackages $ pkgs
  where
    toSimpleDTOs :: [R_PackageSimpleDTO.PackageSimpleDTO] -> [OrganizationSimpleDTO] -> [Package] -> PackageSimpleDTO
    toSimpleDTOs pkgRs orgRs pkgs = toSimpleDTO' pkgRs orgRs (pkgs ^.. traverse . version) newestPkg
      where
        newestPkg = maximumBy (\p1 p2 -> compareVersion (p1 ^. version) (p2 ^. version)) pkgs

getPackagesPage ::
     Maybe String -> Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page PackageSimpleDTO)
getPackagesPage mOrganizationId mKmId mQuery pageable sort = do
  checkPermission _PM_READ_PERM
  groups <- findPackageGroups mOrganizationId mKmId mQuery pageable sort
  iStat <- getInstanceStatistics
  pkgRs <- retrievePackages iStat
  orgRs <- retrieveOrganizations
  return . fmap (toSimpleDTO'' pkgRs orgRs) $ groups

getPackageById :: String -> AppContextM Package
getPackageById = getFromCacheOrDb getFromCache addToCache findPackageById

getPackageDetailById :: String -> AppContextM PackageDetailDTO
getPackageDetailById pkgId = do
  checkPermission _PM_READ_PERM
  serverConfig <- asks _appContextServerConfig
  pkg <- getPackageById pkgId
  versions <- getPackageVersions pkg
  iStat <- getInstanceStatistics
  pkgRs <- retrievePackages iStat
  orgRs <- retrieveOrganizations
  return $ toDetailDTO pkg pkgRs orgRs versions (buildPackageUrl (serverConfig ^. registry . clientUrl) pkgId)

getSeriesOfPackages :: String -> AppContextM [PackageWithEvents]
getSeriesOfPackages pkgId = do
  pkg <- findPackageWithEventsById pkgId
  case pkg ^. previousPackageId of
    Just previousPkgId -> do
      previousPkgs <- getSeriesOfPackages previousPkgId
      return $ previousPkgs ++ [pkg]
    Nothing -> return [pkg]

getAllPreviousEventsSincePackageId :: String -> AppContextM [Event]
getAllPreviousEventsSincePackageId pkgId = do
  package <- findPackageWithEventsById pkgId
  case package ^. previousPackageId of
    Just previousPackageId -> do
      pkgEvents <- getAllPreviousEventsSincePackageId previousPackageId
      return $ pkgEvents ++ (package ^. events)
    Nothing -> return $ package ^. events

getAllPreviousEventsSincePackageIdAndUntilPackageId :: String -> String -> AppContextM [Event]
getAllPreviousEventsSincePackageIdAndUntilPackageId sincePkgId untilPkgId = go sincePkgId
  where
    go pkgId =
      if pkgId == untilPkgId
        then return []
        else do
          package <- findPackageWithEventsById pkgId
          case package ^. previousPackageId of
            Just previousPackageId -> do
              pkgEvents <- go previousPackageId
              return $ pkgEvents ++ (package ^. events)
            Nothing -> return $ package ^. events

getTheNewestPackageByOrganizationIdAndKmId :: String -> String -> AppContextM (Maybe Package)
getTheNewestPackageByOrganizationIdAndKmId organizationId kmId = do
  packages <- findPackagesByOrganizationIdAndKmId organizationId kmId
  if null packages
    then return Nothing
    else do
      let sorted = sortPackagesByVersion packages
      return . Just . head $ sorted

getNewerPackages :: String -> AppContextM [Package]
getNewerPackages currentPkgId = do
  pkgs <- findPackagesByOrganizationIdAndKmId (getOrgIdFromPkgId currentPkgId) (getKmIdFromPkgId currentPkgId)
  let newerPkgs = filter (\pkg -> compareVersion (pkg ^. version) (getVersionFromPkgId currentPkgId) == GT) pkgs
  return . sortPackagesByVersion $ newerPkgs

createPackage :: PackageWithEvents -> AppContextM PackageSimpleDTO
createPackage pkg = do
  insertPackage pkg
  return . toSimpleDTO . toPackage $ pkg

deletePackagesByQueryParams :: [(String, String)] -> AppContextM ()
deletePackagesByQueryParams queryParams = do
  checkPermission _PM_WRITE_PERM
  packages <- findPackagesFiltered queryParams
  let pIds = packages ^.. traverse . pId
  validatePackagesDeletation pIds
  deletePackagesFiltered queryParams
  traverse_ deleteFromCache pIds

deletePackage :: String -> AppContextM ()
deletePackage pkgId = do
  checkPermission _PM_WRITE_PERM
  package <- findPackageById pkgId
  validatePackageDeletation pkgId
  deletePackageById pkgId
  deleteFromCache pkgId

-- --------------------------------
-- PRIVATE
-- --------------------------------
getPackageVersions :: Package -> AppContextM [String]
getPackageVersions pkg = do
  allPkgs <- findPackagesByOrganizationIdAndKmId (pkg ^. organizationId) (pkg ^. kmId)
  return . fmap _packageVersion $ allPkgs
