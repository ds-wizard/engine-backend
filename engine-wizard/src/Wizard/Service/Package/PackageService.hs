module Wizard.Service.Package.PackageService where

import Control.Lens ((^.), (^..))
import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import Data.List (maximumBy)

import LensesConfig
import qualified Registry.Api.Resource.Package.PackageSimpleDTO as R_PackageSimpleDTO
import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Api.Resource.Package.PackageSuggestionDTO
import Shared.Database.DAO.Package.PackageDAO
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Event.Event
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import Shared.Service.Package.PackageMapper hiding (toSuggestionDTO)
import Shared.Service.Package.PackageUtil
import Shared.Util.Coordinate
import Wizard.Api.Resource.Package.PackageDetailDTO
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Package.PackageDAO
import Wizard.Integration.Http.Registry.Runner
import Wizard.Model.Context.AppContext
import Wizard.Service.Acl.AclService
import qualified Wizard.Service.Cache.KnowledgeModelCache as KM_Cache
import qualified Wizard.Service.Cache.PackageCache as PKG_Cache
import Wizard.Service.Limit.AppLimitService
import Wizard.Service.Package.PackageMapper
import Wizard.Service.Package.PackageUtil
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
  pkgs <- findPackagesPage mOrganizationId mKmId mQuery pageable sort
  pkgsWithVersions <- traverse enhance pkgs
  iStat <- getInstanceStatistics
  pkgRs <- retrievePackages iStat
  orgRs <- retrieveOrganizations
  return . fmap (toSimpleDTO'' pkgRs orgRs) $ pkgsWithVersions
  where
    enhance pkg = do
      versions <- findVersionsForPackage (pkg ^. organizationId) (pkg ^. kmId)
      return (pkg, versions)

getPackageSuggestions :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page PackageSuggestionDTO)
getPackageSuggestions mQuery pageable sort = do
  checkPermission _PM_READ_PERM
  pkgs <- findPackagesPage Nothing Nothing mQuery pageable sort
  pkgsWithVersions <- traverse enhance pkgs
  return . fmap toSuggestionDTO $ pkgsWithVersions
  where
    enhance pkg = do
      versions <- findVersionsForPackage (pkg ^. organizationId) (pkg ^. kmId)
      return (pkg, versions)

getPackageById :: String -> AppContextM Package
getPackageById pkgId =
  resolvePackageId pkgId >>= getFromCacheOrDb PKG_Cache.getFromCache PKG_Cache.addToCache findPackageById

getPackageDetailById :: String -> AppContextM PackageDetailDTO
getPackageDetailById pkgId = do
  resolvedPkgId <- resolvePackageId pkgId
  checkIfPackageIsPublic (Just resolvedPkgId) _PM_READ_PERM
  serverConfig <- asks _appContextServerConfig
  pkg <- getPackageById resolvedPkgId
  versions <- getPackageVersions pkg
  iStat <- getInstanceStatistics
  pkgRs <- retrievePackages iStat
  orgRs <- retrieveOrganizations
  return $ toDetailDTO pkg pkgRs orgRs versions (buildPackageUrl (serverConfig ^. registry . clientUrl) (pkg ^. pId))

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
  pkgs <- findPackagesByOrganizationIdAndKmId (getOrgIdFromCoordinate currentPkgId) (getKmIdFromCoordinate currentPkgId)
  let newerPkgs = filter (\pkg -> compareVersion (pkg ^. version) (getVersionFromCoordinate currentPkgId) == GT) pkgs
  return . sortPackagesByVersion $ newerPkgs

createPackage :: PackageWithEvents -> AppContextM PackageSimpleDTO
createPackage pkg =
  runInTransaction $ do
    checkPackageLimit
    insertPackage pkg
    return . toSimpleDTO . toPackage $ pkg

deletePackagesByQueryParams :: [(String, String)] -> AppContextM ()
deletePackagesByQueryParams queryParams =
  runInTransaction $ do
    checkPermission _PM_WRITE_PERM
    packages <- findPackagesFiltered queryParams
    let pIds = packages ^.. traverse . pId
    validatePackagesDeletation pIds
    deletePackagesFiltered queryParams
    traverse_ PKG_Cache.deleteFromCache pIds
    traverse_ KM_Cache.deleteFromCache' pIds

deletePackage :: String -> AppContextM ()
deletePackage pkgId =
  runInTransaction $ do
    checkPermission _PM_WRITE_PERM
    package <- findPackageById pkgId
    validatePackageDeletation pkgId
    deletePackageById pkgId
    PKG_Cache.deleteFromCache pkgId
    KM_Cache.deleteFromCache' pkgId

-- --------------------------------
-- PRIVATE
-- --------------------------------
getPackageVersions :: Package -> AppContextM [String]
getPackageVersions pkg = do
  allPkgs <- findPackagesByOrganizationIdAndKmId (pkg ^. organizationId) (pkg ^. kmId)
  return . fmap _packageVersion $ allPkgs
