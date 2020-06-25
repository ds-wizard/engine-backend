module Wizard.Service.Package.PackageService
  ( getSimplePackagesFiltered
  , getPackageById
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
import Data.List (maximumBy)

import LensesConfig
import qualified Registry.Api.Resource.Package.PackageSimpleDTO as R_PackageSimpleDTO
import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Model.Event.Event
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import Shared.Service.Package.PackageMapper
import Shared.Util.List (groupBy)
import Wizard.Api.Resource.Package.PackageDetailDTO
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Database.DAO.Package.PackageDAO
import Wizard.Integration.Http.Registry.Runner
import Wizard.Model.Context.AppContext
import Wizard.Service.Common.ACL
import Wizard.Service.Package.PackageMapper
import Wizard.Service.Package.PackageUtils
import Wizard.Service.Package.PackageValidation
import Wizard.Service.Statistics.StatisticsService
import Wizard.Util.IdentifierUtil

getSimplePackagesFiltered :: [(String, String)] -> AppContextM [PackageSimpleDTO]
getSimplePackagesFiltered queryParams = do
  checkPermission _PM_READ_PERM
  pkgs <- findPackagesFiltered queryParams
  iStat <- getInstanceStatistics
  pkgRs <- retrievePackages iStat
  orgRs <- retrieveOrganizations
  return . fmap (toSimpleDTOs pkgRs orgRs) . groupPkgs $ pkgs
  where
    groupPkgs :: [Package] -> [[Package]]
    groupPkgs = groupBy (\p1 p2 -> (p1 ^. organizationId) == (p2 ^. organizationId) && (p1 ^. kmId) == (p2 ^. kmId))
    toSimpleDTOs :: [R_PackageSimpleDTO.PackageSimpleDTO] -> [OrganizationSimpleDTO] -> [Package] -> PackageSimpleDTO
    toSimpleDTOs pkgRs orgRs pkgs = toSimpleDTO' newestPkg pkgRs orgRs (pkgs ^.. traverse . version)
      where
        newestPkg = maximumBy (\p1 p2 -> compareVersion (p1 ^. version) (p2 ^. version)) pkgs

getPackageById :: String -> AppContextM PackageDetailDTO
getPackageById pkgId = do
  checkPermission _PM_READ_PERM
  serverConfig <- asks _appContextServerConfig
  pkg <- findPackageById pkgId
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
  validatePackagesDeletation (_packagePId <$> packages)
  deletePackagesFiltered queryParams

deletePackage :: String -> AppContextM ()
deletePackage pkgId = do
  checkPermission _PM_WRITE_PERM
  package <- findPackageById pkgId
  validatePackageDeletation pkgId
  deletePackageById pkgId

-- --------------------------------
-- PRIVATE
-- --------------------------------
getPackageVersions :: Package -> AppContextM [String]
getPackageVersions pkg = do
  allPkgs <- findPackagesByOrganizationIdAndKmId (pkg ^. organizationId) (pkg ^. kmId)
  return . fmap _packageVersion $ allPkgs
