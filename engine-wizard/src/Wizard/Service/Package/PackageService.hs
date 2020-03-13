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

import Control.Lens ((^.), (^..), traverse)
import Control.Monad.Reader (asks)
import Data.List (maximumBy)

import LensesConfig
import Shared.Model.Event.Event
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import Wizard.Api.Resource.Package.PackageDetailDTO
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Database.DAO.Package.PackageDAO
import Wizard.Integration.Http.Registry.Runner
import Wizard.Integration.Resource.Package.PackageSimpleIDTO
import Wizard.Model.Context.AppContext
import Wizard.Service.Package.PackageMapper
import Wizard.Service.Package.PackageUtils
import Wizard.Service.Package.PackageValidation
import Wizard.Service.Statistics.StatisticsService
import Wizard.Util.List (groupBy)

getSimplePackagesFiltered :: [(String, String)] -> AppContextM [PackageSimpleDTO]
getSimplePackagesFiltered queryParams = do
  serverConfig <- asks _appContextApplicationConfig
  pkgs <- findPackagesFiltered queryParams
  iStat <- getInstanceStatistics
  pkgRs <- retrievePackages (serverConfig ^. registry) iStat
  return . fmap (toSimpleDTOs pkgRs) . groupPkgs $ pkgs
  where
    groupPkgs :: [Package] -> [[Package]]
    groupPkgs = groupBy (\p1 p2 -> (p1 ^. organizationId) == (p2 ^. organizationId) && (p1 ^. kmId) == (p2 ^. kmId))
    toSimpleDTOs :: [PackageSimpleIDTO] -> [Package] -> PackageSimpleDTO
    toSimpleDTOs pkgRs pkgs = toSimpleDTO' newestPkg pkgRs (pkgs ^.. traverse . version)
      where
        newestPkg = maximumBy (\p1 p2 -> compareVersion (p1 ^. version) (p2 ^. version)) pkgs

getPackageById :: String -> AppContextM PackageDetailDTO
getPackageById pkgId = do
  serverConfig <- asks _appContextApplicationConfig
  pkg <- findPackageById pkgId
  versions <- getPackageVersions pkg
  iStat <- getInstanceStatistics
  pkgRs <- retrievePackages (serverConfig ^. registry) iStat
  return $ toDetailDTO pkg pkgRs versions (buildPackageUrl (serverConfig ^. registry . clientUrl) pkgId)

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
  packages <- findPackagesFiltered queryParams
  validatePackagesDeletation (_packagePId <$> packages)
  deletePackagesFiltered queryParams

deletePackage :: String -> AppContextM ()
deletePackage pkgId = do
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
